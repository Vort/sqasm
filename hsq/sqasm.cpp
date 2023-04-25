// Oleg Mazonka: 10 Nov 2006; 22 Jan 2009; 11 Sept 2009
// Modifications by Vort

#include <iostream>
#include <fstream>
#include <cstdint>
#include <string>
#include <vector>
#include <map>
#include <ctype.h>

using namespace std;

int error_status = 0;

int str2int(const string &s, int size)
{
    long long maxInt = size == 4 ? 0x100000000 : 0x100;

    long long x;
    if (s.length() >= 2 && s[1] == 'x')
        x = strtoll(s.c_str() + 2, nullptr, 16);
    else
        x = strtoll(s.c_str(), nullptr, 10);
    if (x < maxInt)
        return (int)x;
    else
        throw 0;
}

string int2str(int x, int pr = 0)
{
    char buf[40];
    sprintf(buf, "%d", x);
    if ( !pr ) return buf;

    string s = buf;
    while ( s.size() < (unsigned)pr ) s = string("0") + s;
    return s;
}

/* grammar
program := list of intructions
intruction := [.] list of items ( ';' | '\n' )
item    := [label:]expression
label   := id
expression := ( term | term+expression | term-expression )
term    := ( -term | (expression) | const | id )
const   := ( number | 'letter' | ? )
*/

struct item
{
    int size;
    int addr;
    string s;
    int i;
    enum { EMPTY, STR, RES } state;
    item(int size) : state(EMPTY), size(size) {}
    void dump(ofstream& out);
};

void item::dump(ofstream& out)
{
    if (state == RES)
        out.write((char*)&i, size);
    else
    {
        cerr << "Unresolved symbol '" << s << "'\n";
        throw 0;
    }
}

struct instruction
{
    vector<item> items;
    void dump(ofstream& out);
};

void instruction::dump(ofstream& out)
{
    for (size_t i = 0; i<items.size(); i++)
        items[i].dump(out);
}

map<string, int> lab2adr;
string prog;
typedef string::size_type sint;
sint pip = 0;
int line = 1;
int addr = 0;
map<string, int> unres;

void eat()
{
    while ( pip < prog.size() && (isspace(prog[pip]) &&  prog[pip] != '\n') )
        pip++;

    if ( prog[pip] == '#' )
        while ( pip < prog.size() && prog[pip] != '\n' )
            pip++;
}
void eatn() { eat(); while (prog[pip] == '\n' ) { pip++; line++; eat(); } }


void getid(string & s)
{
    eat();
    if ( isalpha(prog[pip]) || prog[pip] == '_' )
    {
        s += prog[pip];
        while ( pip < prog.size() &&
                ( isalnum(prog[++pip]) || prog[pip] == '_' ) ) s += prog[pip];
    }
}

void getid(item & i)
{
    getid(i.s);

    i.state = item::STR;
    if ( lab2adr.find(i.s) != lab2adr.end() )
    {
        i.i = lab2adr[i.s];
        i.state = item::RES;
    }
}

char getChr(int chr = false)
{
    if ( prog[++pip] != '\\' )
    {
        return prog[pip];
    }
    else          // escape symbol
    {
        switch (prog[++pip])
        {
            case 'n': return '\n';
            case '\\': return '\\';
            case '"': return '"';
            case '\'': return '\'';
            case '0': return '\0';
        }
    }

    cerr << "Warning " << line << " unknown escape char '" << prog[pip] << "'\n";
    if (!chr) pip--;
    return prog[pip];
}

bool getconst(item & i)
{
    eat();
    if ( prog[pip] == '\'' )
    {
        i.i = (unsigned char)getChr(true);
        if ( prog[++pip] != '\'' )
        {
            cerr << "Error " << line << " closing ' expected was '" << prog[pip] << "'\n";
            error_status = __LINE__;
            return false;
        }
        i.state = item::RES;
        pip++;
        return true;
    }

    if ( prog[pip] == '?' )
    {
        i.i = addr;
        i.state = item::RES;
        pip++;
        return true;
    }

    if ( !isdigit(prog[pip]) ) return false;
    while (
        pip<prog.size() &&
        (
            isdigit(prog[pip]) ||
            prog[pip] == 'x' ||
            (prog[pip] >= 'A' && prog[pip] <= 'F') ||
            (prog[pip] >= 'a' && prog[pip] <= 'f')
            )) i.s += prog[pip++];
    i.i = str2int(i.s, i.size);
    i.state = item::RES;
    return true;
}

void getexpr(item & i);

void getterm(item & i)
{
    eat();

    if ( prog[pip] == '-' )
    {
        pip++;
        getterm(i);
        i.i = -i.i;
        return;
    }

    if ( prog[pip] == '(' )
    {
        pip++;
        getexpr(i);
        if ( prog[pip] != ')' )
        {
            cerr << "Error " << line << " closing ) expected\n";
            error_status = __LINE__;
        }
        else pip++;
        return;
    }

    if ( getconst(i) ) return;
    getid(i);
}

void getexpr(item & i)
{
    eat();

    getterm(i);

tryterm:
    eat();
    if ( prog[pip] == '-' )
    {
        pip++;
        item j(i.size);
        getterm(j);
        if ( j.state == item::RES && i.state == item::RES ) i.i -= j.i;
        else
        {
            if ( i.state == item::RES ) i.s = int2str(i.i);
            if ( j.state == item::RES ) j.s = int2str(j.i);
            i.s = i.s + "-" + j.s;
            i.state = item::STR;
        }
        goto tryterm;
    }

    if ( prog[pip] == '+' )
    {
        pip++;
        item j(i.size);
        getterm(j);
        if ( j.state == item::RES && i.state == item::RES ) i.i += j.i;
        else
        {
            if ( i.state == item::RES ) i.s = int2str(i.i);
            if ( j.state == item::RES ) j.s = int2str(j.i);
            i.s = i.s + "+" + j.s;
            i.state = item::STR;
        }
        goto tryterm;
    }

    if (prog[pip] == '*')
    {
        pip++;
        item j(i.size);
        getterm(j);
        if (j.state == item::RES && i.state == item::RES) i.i *= j.i;
        else
        {
            if (i.state == item::RES) i.s = int2str(i.i);
            if (j.state == item::RES) j.s = int2str(j.i);
            i.s = i.s + "*" + j.s;
            i.state = item::STR;
        }
        goto tryterm;
    }
}

bool getlabel(string & s)
{
    sint mypip = pip;
    getid(s);
    if ( prog[pip] == ':' )
    {
        pip++;
        eatn();
        return true;
    }
    pip = mypip;
    return false;
}

bool getStr(item & i)
{
    static int j = 0;

    if ( j == 0 ) j++; // start

    if ( pip + j >= prog.size() )
    {
        cerr << "Error " << line << " string not closed\n";
        error_status = __LINE__;
        pip += j;
        return false;
    }

    if ( prog[pip + j] == '"' ) // end
    {
        pip += j + 1;
        j = 0;
        return false;
    }

    {
        int p0 = pip;
        pip += j - 1;
        i.i = (unsigned char)getChr();
        j += pip - j + 1 - p0;
        pip = p0;
    }

    i.state = item::RES;

    return true;

}

bool getitem(item & i)
{
begin:
    eat();
    if ( prog[pip] == '\n' ) { pip++; line++; return false; }
    if ( prog[pip] == ';' ) { pip++; return false; }

    if ( pip >= prog.size() ) return false;

    string lab;
    while ( getlabel(lab) )
    {
        if ( lab2adr.find(lab) == lab2adr.end() )
            lab2adr[lab] = addr;
        else
        {
            cerr << "Error " << line << ": label " << lab << " was defined\n";
            error_status = __LINE__;
        }
        lab = "";
    }

    eat();
    i.addr = addr += i.size;

    if ( prog[pip] == '"' )
    {
        if ( getStr(i) ) return true;
        addr -= i.size; // finished with string - try again
        goto begin;
    }

    getexpr(i);

    return true;
}

bool getinstr(instruction & i)
{
    eatn();
    bool data = false;
    int itemSize = 4;
    if (prog[pip] == '.') { data = true; pip++; }
    if (prog[pip] == '!') { data = true; pip++; itemSize = 1; }
    while ( pip < prog.size() )
    {
        sint pip0 = pip;
        item t(itemSize);
        if ( getitem(t) ) i.items.push_back(t);
        else if ( i.items.size() == 0 ) continue;
        else break;

        if ( pip0 == pip && t.state == item::STR && t.s.empty() )
            throw string() + "Syntax error";

        if ( i.items.size() > 10000000 )
            throw string() + "Sqasm (I): instruction size hardcoded limit exceeded."
            " This is an artificial limit added for security purposes."
            " Remove the limit inside the compiler if necessary.";
    }

    if ( i.items.size() == 0 ) return false;

    if ( i.items.size() == 1 )
    {
        item & k = i.items.front();
        if ( k.state == item::STR && k.s == "" ) return false;
    }

    if ( !data && i.items.size() == 1 )
    {
        item k = i.items.front();
        k.addr = addr += itemSize;
        i.items.push_back(k);
    }

    if ( !data && i.items.size() == 2 )
    {
        item k(itemSize);
        k.addr = addr += itemSize;
        k.i = addr;
        k.state = item::RES;
        i.items.push_back(k);
    }

    return true;
}

vector<instruction> program()
{
    vector<instruction> r;
    while ( pip < prog.size() )
    {
        instruction i;
        if ( getinstr(i) ) r.push_back(i);
        else break;
    }
    return r;
}

void resolve(item & i)
{
    if ( i.state == item::EMPTY )
    {
        cerr << "Internal Error: empty item\n";
        error_status = __LINE__;
        return;
    }

    if ( i.state == item::RES ) return;

    prog = i.s;
    pip = 0;
    item k(i.size);
    getitem(k);
    if ( k.state == item::RES )
    {
        i.state = item::RES;
        i.i = k.i;
        return;
    }

    i.s = k.s;
    unres[i.s] = i.addr;
}

void resolve(instruction & n)
{
    for ( size_t i = 0; i < n.items.size(); i++ )
        resolve(n.items[i]);
}

void resolve(vector<instruction> & pr)
{
    for ( size_t i = 0; i < pr.size(); i++ )
    {
        resolve(pr[i]);
    }
}

int main(int argc, char *argv[])
try
{
    if (argc != 3)
    {
        cout << "usage: sqasm input_file.asq output_file.sqb" << endl;
        return 0;
    }

    ifstream ifs(argv[1]);
    while (1)
    {
        string s;
        getline(ifs, s);
        prog += s + '\n';
        if (!ifs) break;
    }

    vector<instruction> pr = program();

    if ( error_status ) return error_status;

    ofstream ofs(argv[2], ofstream::out | ofstream::binary);
    for ( size_t i = 0; i < pr.size(); i++ )
    {
        resolve(pr[i]);
        pr[i].dump(ofs);
    }

    if ( unres.size() )
    {
        cerr << "Warning: unresolved symbols: ";
        for ( map<string, int>::iterator i = unres.begin(); i != unres.end(); i++ )
            cerr << " [" << i->first << ":" << i->second << "]";
        cerr << '\n';
    }
}
catch (string e)
{
    std::cerr << "Error in line " << line << " : " << e << '\n';
    return 1;
}
catch (...)
{
    std::cerr << "Unknown exception" << '\n';
    return 1;
}
