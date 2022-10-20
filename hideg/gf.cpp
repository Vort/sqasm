#include <iostream>
#include <fstream>
#include <filesystem>

#include "olu.h"
#include "hash.h"
#include "gfu.h"

using std::cout;
namespace fs = std::filesystem;

using vs = ivec<string>;

int main_bzc(vs args);
int main_test(vs args);
int main_hid(vs args);
int main_pack(vs args, bool pack);
int main_fcl(vs args);
int main_info(vs args);
int main_sync(vs args, int sync_co_st);

string g_ver = "gf, ver 1.1.0, Oleg Mazonka 2022";

int main(int ac, const char * av[])
try
{
    ivec<string> args;
    for (int i = 1; i < ac; i++) args += av[i];
    int sz = args.size();
    if ( sz < 0 ) never;
    if ( sz < 1 )
    {
        //cout << g_ver << "\n";
        cout << "Usage: bzc, g, test, pack/unpack, fcl, "
             "info [file], sync/co/st/clean [@][path|file]\n";
        return 0;
    }
    auto cmd = args[0];
    args.erase(0);

    g::gfexe = fs::path(av[0]);
    g_keyfilename = "." + g::gfexe.stem().string() + ".key";

    if (0) {}
    else if ( cmd == "bzc" ) return main_bzc(args);
    else if ( cmd == "test" ) return main_test(args);
    else if ( cmd == "g" ) return main_hid(args);
    else if ( cmd == "pack" ) return main_pack(args, true);
    else if ( cmd == "unpack" ) return main_pack(args, false);
    else if ( cmd == "fcl" ) return main_fcl(args);
    else if ( cmd == "info" ) return main_info(args);
    else if ( cmd == "sync" ) return main_sync(args, 1);
    else if ( cmd == "co" ) return main_sync(args, 2);
    else if ( cmd == "st" ) return main_sync(args, 3);
    else if ( cmd == "clean" ) return main_sync(args, 4);


    throw "Bad command: " + cmd;
}

catch (int e)
{
    cout << "error (int): " << e << "\n";
    return 1;
}
catch (string e)
{
    cout << "error: " << e << "\n";
    return 1;
}
catch (const char * e)
{
    cout << "error: " << e << "\n";
    return 1;
}
catch (std::exception & e)
{
    cout << "error (std): " << e.what() << "\n";
    return 1;
}
catch (...)
{
    cout << "Unknown exception\n";
    return 1;
}



int main_test(ivec<string> avs)
{
    ///if ( arg0.empty() ) never;
    if ( avs.size() < 1 )
    {
        main_test(avs + "bzc");
        return 0;
    }

    if ( avs.size() != 1 ) never;

    string mod = avs[0];
    ///avs.pop_back();

    cout << "testing " << mod << '\n';
    if ( mod == "bzc" )
    {
        if ( main_bzc({}) ) throw "bad key";

        string fname = "gf.test.tmp";
        string fnameZ = fname + ".bz2";
        string fnameC = fname + ".bzc";

        {
            std::ofstream of(fname, std::ios::binary);
            for ( int i = 0; i < 10000; i++ )
                of.write((const char *)&i, sizeof(int));
        }

        string file_content = ol::file2str(fname);
        cout << "file size " << file_content.size() << '\n';


        ol::delfile(fnameZ);
        if ( ol::bzip(fname, true) ) throw "Cannot start bzip2";
        ol::delfile(fname);
        int err = main_bzc(vs() + "enc" + fnameZ);
        if ( err ) throw "encrypt fail";
        if ( !ol::delfile(fnameZ) ) throw "Cannot delete " + fnameZ;

        if ( main_bzc(vs() + "dec" + fnameC) ) throw "decrypt fail";
        if ( !ol::delfile(fnameC) ) throw "Cannot delete " + fnameC;
        ol::bzip(fnameZ, false);

        string file_content2 = ol::file2str(fname);

        ol::delfile(fname);

        if ( file_content == file_content2 ) cout << "all good\n";
        else throw "FAILED";

        return 0;
    }

    nevers("unknown test module");
}

int main_pack(vs args, bool pack)
{
    ///if ( arg0.empty() ) never;
    if ( args.size() < 1 )
    {
        cout << "use filename\n";
        return 0;
    }

    if ( args.size() != 1 ) throw "need 1 filename";

    string fname = args[0];

    if ( main_bzc({}) ) throw "bad key";

    bool isdir = true;
    if ( fs::is_regular_file(fname) ) isdir = false;
    else if ( fs::is_directory(fname) ) {}
    else throw "no file or dir [" + fname + "]";

    if (pack)
    {
        if (isdir)
        {
            if ( main_fcl(vs() + "make" + fname) ) throw "fcl fail";
            fname += ".fcl";
        }

        string fnameZ = fname + ".bz2";
        if ( ol::bzip(fname, true) ) throw "bzip2 fail";
        if ( main_bzc(vs() + "enc" + fnameZ) ) throw "encrypt fail";
        if ( !ol::delfile(fnameZ) ) throw "Cannot delete " + fnameZ;
    }
    else // unpack
    {
        if ( isdir ) throw "[" + fname + "] is dir";

        static int reent = 0;

        string fncut;

        if (0) {}
        else if ( ol::endsWith(fname, ".bzc", fncut) )
        {
            if ( main_bzc(vs() + "dec" + fname) ) throw "decrypt fail";
            if ( !ol::delfile(fname) ) throw "Cannot delete " + fname;
            ///fname = fname.substr(0, fname.size() - 4);
            fname = fncut;
            ///ol::bzip( fname + ".bz2", false);
            fname += ".bz2";
        }
        else if ( ol::endsWith(fname, ".fcl") )
        {
            if ( main_fcl(vs() + "extr" + fname) ) throw "fcl fail";
            if ( !ol::delfile(fname) ) throw "Cannot delete " + fname;
            return 0; // no descent after fcl
        }
        else if ( ol::endsWith(fname, ".bz2", fncut) )
        {
            ol::bzip( fname, false);
            ///fname = fname.substr(0, fname.size() - 4);
            fname = fncut;
        }
        else if ( ol::endsWith(fname, ".g", fncut) )
        {
            main_hid(vs() + fname);
            if ( !ol::delfile(fname) ) throw "Cannot delete " + fname;
            ///fname = fname.substr(0, fname.size() - 2);
            fname = fncut;
        }
        else
        {
            if ( reent == 0 ) throw "file is unpackable";
            return 0; // finish recursion
        }

        ++reent;
        int ret = main_pack(vs() + fname, false);
        --reent;
        return ret;
    }

    return 0;
}

int main_info(vs args)
{
    cout << g_ver << '\n';

    main_bzc({});
    extern fs::path g_keyfile;
    cout << "Keyfile = " << g_keyfile.string() << '\n';

    if ( args.empty() ) return 0;

    string file = args[0];

    ///std::error_code err;
    ///auto lwt = 1ull * fs::last_write_time(file, err).time_since_epoch().count();
    ///bool ok = !err.default_error_condition();
    auto lwt = ol::filetime(file);
    if ( lwt == 0 )
    {
        cout << "Cannot access file " << file << "\n";
        return 1;
    }
    cout << "LWT = " << lwt << "\n";

    if ( !fs::is_regular_file(file) ) return 0;


    ///string body = ol::file2str(file);
    ///string hash = ha::hashHex(body);
    string hash = gfu::fileHash(file);
    cout << "hash = " << hash << "\n";

    ///if( !ol::endsWith(file,".bzc") ) return 0;

    return 0;
}

