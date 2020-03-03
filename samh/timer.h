#pragma once

#include <string>

#include "gl_defs.h"

using std::string;


class Timer
{
        gl::sll tv_sec, tv_usec;
        gl::sll adj;

    public:
        void init();
        Timer(): tv_sec(0), tv_usec(0), adj(0) { init(); }
        Timer(int): tv_sec(0), tv_usec(0), adj(0) {} // no init
        gl::sll get(); // get time passed in millisecs
        void set(gl::sll);

        // date +%g%m%d
        static string getGmd();
        static void setGmd(string gmd);
        static string getHms();
        static void setHms(string hms);
};




