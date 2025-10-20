#include <ncurses.h>

/* Helper function to get terminal dimensions
   Since getmaxyx is a macro, we need a C function to call it */
void getmaxyx_helper(int *y, int *x) {
    *y = LINES;
    *x = COLS;
}
