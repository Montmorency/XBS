The code base has a relatively simple structure. The X11 system and the main function are located in xbs.c.

grsubs.h subroutines for graphics:

parse_color
getColorGC
SetColors
FreeColorGC
NewSpecColor
SetSmoothGrays (XSetForeground, XAllocColor)
SetStippled4x4
SetStippled4x6
showline (XFillRectangle)
clearline
DrawArrow (XDrawLine, XDrawString)
DrawLine
DrawBall (XDrawArc (if unfilled atom), XFillArc (if filled atoms))
DrawStick
if (shadow)
XDrawLine (dpy, drw, shadowgc, x1, y1, x2, y2);
XDrawLine (dpy, drw, gc, x1, y1, x2, y2);
XFillPolygon
XDrawLines
LabelBG
Alot of the calls to XDrawline, XDrawString in grsubs.c are what we want to replace using js. The only modifications required to existing libraries will be if we want to alter linestyles to get closer to the classic aesthetic.

hardcopy.h subroutines to set ball and stick mode For printing to file.

HCballFull
HCballWire
HCballShadow
HCstickFull
HCstickShadow
HCstickWire
HCstickLine
HCstickLineShadow
HCdbond
HCdbondShadow
hardcopy_init
hardcopy_redefine
hardcopy_ball
hardcopy_label
hardcopy_stick
hardcopy_line
hardcopy_close
hardcopy_xdbond
hardcopy_ydbond
For printing purposes it is probably better to just export the current view in some structured format and write a small program that calls the existing c functions to output an eps.

subs.h (useful subroutines):

rx print error and exit
cross cross product
sp dot product of two vecs
vscal scalar product
vsum (aV1 + bV2)
parse args
strip
abbrev
strext (set extension on a file identifier)
match (matches keyboard input to function patterns)
get_extent (find size of an array of points in a window)
parse_all_colors
set_auto_colors
getframe
putframe
prframes
We just need to find the right js equiv library that contains all the relevant vector operations. If no lightweight one exists we can write one can quickly. The strip parse args can all be handled using the native observable event handlers like buttons forms input bars etc. The key program logic is contained in:

atompos ( position and radius on paper for an atom: three modes of perspective)
readclusterdata (read clusters we will pass around via json.)
readclusterline (read configuration options: blackwhite mode, bonds, greyscale, scale spheres, etc.)
writeclusterdata
ball_list
stick_list
duplicate_atoms ("Usage: dup vx vy vz - duplicate shifted by vector")
cut_atoms ("Usage: cut vx vy vz a b - cut along vector at a and b")
selectbonds
rotmat (rotation matrix)
eumat (euler matrix: sets tmat transformation matrix)
dbond
draw_lines
draw_axes
bs_transform (uses tmat)
bs_kernel (Important! sort atoms back to front, make list of sphere centers and radii, start loop over atoms plot ball first DrawBall, make list of bonds to atom, inner loop over bonds, plots sticks DrawStick, writes bond lengths if desired.
The main program is contained in xbs.c. The following libraries are imported:

    27 #include <stdio.h>
    28 #include <stdlib.h>
    29 #include <signal.h>
    30 #include <X11/X.h>
    31 #include "X11/Xlib.h"
    32 #include <X11/Xutil.h>
    33 #include <X11/keysym.h>
    34 #include <math.h>
    35 #include <time.h>
    36 #include <strings.h>
All the X11 is getting js-ed. Actually all the libraries are. Then there are a number of startup document size parameters, and return codes for key presses which again we will be replacing with browser based/observable/js friendly interactions. XBS is X-11 ball and stick. So we are really just drawing balls and sticks with X-11. Hence; two important structs are:

    74 struct ballstr {
    75   float pos[3];
    76   float rad;
    77   float gray;
    78   float r,g,b;
    79   int col;
    80   int special;
    81   char lab[21];
    82 };

    84 struct stickstr {
    85   int start;
    86   int end;
    87   float rad;
    88   float gray;
    89   int col;
    90 };
A number of other global arrays are initialized for the species, atoms, bonds and generic lines:

    94 struct {
    95   char  lab[21];
    96   float rad;
    97   float r,g,b;
    98   char  cname[81];
    99   int   col;
   100   float gray;
   101 } spec[NSPMAX];
   102
   103 struct {
   104   char lab[21];
   105   float pos[3];
   106   float pol[3];
   107 } atom[NAMAX];
   108
   109 struct {
   110   char  lab1[21];
   111   char  lab2[21];
   112   float min;
   113   float max;
   114   float rad;
   115   float r,g,b;
   116   char  cname[81];
   117   int   col;
   118   float gray;
   119 } bonds [NBTMAX];
   120
   121 struct {
   122   float a[3],b[3];
   123 } xline [NLNMAX];    /* for extra lines */

   125 int natom,nbond;
   126 struct ballstr  ball[NAMAX];
   127 struct stickstr stick[NBMAX];
   128
   129 float arc[NPOINTS][2],xbot,xtop,ybot,ytop;
The default limits for the max size of various arrays are:

    40 #define  NAMAX       2000
    41 #define  NBMAX       8000
    42 #define  NBTMAX       200
    43 #define  NSPMAX        50
    44 #define  NLNMAX        50
Additional X11, printing, event, and window update logic is included in the xbs.c file.

do_ConfigureNotify (new pixmaps, centering on window resize)
close_print, handle_print, hardcopy_close
update_from_file
interpret_input (executes a number of the useful subroutines in subs.h; the reactive notebooks mean these transformations are imbedded.)
main while loop that handles cases e.g. KeyPress, FileUpdate (we could take coords from API on a server where calculations are running which would be cool, replot, etc.) *interpret_keypress again the reactive nature of the notebook should be iso to the logic based on key presses.
wln *WriteHelp,WriteInfo, WriteStatus
extradata for positions

An Input File looks like

  1 * Saved Wed Mar  6 19:30:42 1996 from ring.bs
  2
  3 atom      C      -1.314     -0.015      6.623
  4 atom      C       1.331     -0.014      6.623
  5 atom      C       6.635     -1.316      0.005
  6 atom      C       6.633      1.329      0.011
  7 atom      C       0.004      6.619      1.344
  8 atom      C       0.004      6.627     -1.300
  9 atom      C       1.332      0.019     -6.609
                      .....
 65
 66 spec      C      1.000   .67
 67 spec      O      1.000   .50
 68
 69 bonds     C     C    0.000    3.000    0.143   1.00
 70 bonds     C     O    0.000    3.000    0.143   1.00
 71 bonds     O     O    0.000    3.000    0.143   1.00
 72
 73 tmat  1.000  0.000  0.000  0.000  1.000  0.000  0.000  0.000  1.000
 74 dist    51.629
 75 inc      5.000
 76 scale   15.671
 77 rfac 1.00
 78 bfac 1.00
 79 pos    0.000    0.000
 80 switches 1 0 1 0 0 1 1 0 0
From the README we see:

 This sets the coordinates in the format
 43   atom  species  x  y  z
 44
 45 and how to draw each atomic species, in the format
 46   spec name radius color
 47
 48 and how to draw bonds, in the format
 49   bonds name1 name2 min-length max-length radius color
As mentioned we may also have a .mv file which is a serialized way of updating the coordinates so that we can view animations. A .mv file frame has coordinates and a little meta data:

 1 frame t=    .000 T=     .0  V=-8848.5  T+V=-8848.5
  2 -1.314 -0.015 6.623 1.331 -0.013 6.623 6.635 -1.317 0.005 6.633 1.329 0.011 0.004 6.619 1.344 0.005 6.627 -1.300 1.331 0.019 -6.609 -1.3    13 0.018 -6.610 -6.616 1.321 0.009 -6.614 -1.325 0.003 0.014 -6.615 -1.331 0.013 -6.622 1.313 2.714 2.229 5.774 2.245 5.761 2.727 5.775     2.708 2.254 -2.701 2.226 5.773 -5.761 2.701 2.252 -2.236 5.758 2.726 -2.698 -2.254 5.763 -5.757 -2.715 2.239 -2.228 -5.771 2.699 2.718 -    2.251 5.764 2.254 -5.768 2.700 5.779 -2.708 2.241 -2.697 -2.225 -5.761 -2.228 -5.757 -2.714 -5.757 -2.704 -2.241 2.718 -2.222 -5.759 5.7    79 -2.697 -2.239 2.254 -5.755 -2.713 2.715 2.258 -5.749 5.775 2.719 -2.226 2.246 5.775 -2.686 -2.700 2.255 -5.751 -2.236 5.773 -2.687 -5    .761 2.712 -2.228 1.390 4.369 4.961 4.956 1.378 4.388 4.387 4.949 1.403 -4.940 1.372 4.387 -1.379 4.367 4.961 -4.376 4.943 1.402 -4.939     -1.396 4.381 -1.373 -4.390 4.941 -4.369 -4.951 1.378 1.396 -4.388 4.941 4.394 -4.947 1.380 4.958 -1.390 4.382 -1.372 -4.365 -4.948 -4.93    8 -1.374 -4.376 -4.369 -4.944 -1.390 4.958 -1.368 -4.373 1.396 -4.364 -4.948 4.394 -4.939 -1.388 4.956 1.400 -4.367 1.391 4.394 -4.927 4    .387 4.956 -1.365 -1.378 4.393 -4.928 -4.376 4.950 -1.366 -4.940 1.394 -4.369 5.800 0.000 2.200 8.050 0.000 3.040
  3
  4 frame t=   1.675 T=  200.0  V=-9066.4  T+V=-8866.4
  5 -1.313 -0.015 6.624 1.331 -0.013 6.623 6.626 -1.350 -0.043 6.624 1.363 -0.037 0.006 6.619 1.346 0.004 6.626 -1.301 1.332 0.019 -6.609 -1    .313 0.018 -6.610 -6.616 1.322 0.009 -6.614 -1.326 0.003 0.013 -6.615 -1.332 0.015 -6.622 1.315 2.730 2.232 5.769 2.242 5.762 2.727 5.75    7 2.759 2.238 -2.701 2.225 5.774 -5.761 2.701 2.251 -2.236 5.759 2.727 -2.698 -2.253 5.763 -5.757 -2.715 2.238 -2.228 -5.771 2.699 2.733     -2.253 5.758 2.250 -5.769 2.700 5.761 -2.759 2.225 -2.698 -2.224 -5.761 -2.226 -5.758 -2.713 -5.756 -2.705 -2.240 2.720 -2.220 -5.759 5    .790 -2.687 -2.231 2.255 -5.755 -2.713 2.717 2.257 -5.749 5.786 2.709 -2.219 2.247 5.776 -2.686 -2.701 2.254 -5.751 -2.235 5.773 -2.687     -5.760 2.713 -2.227 1.387 4.370 4.961 4.916 1.396 4.425 4.395 4.941 1.414 -4.939 1.372 4.389 -1.377 4.369 4.961 -4.376 4.943 1.402 -4.93    8 -1.396 4.382 -1.371 -4.391 4.941 -4.369 -4.951 1.378 1.393 -4.390 4.940 4.402 -4.939 1.390 4.918 -1.407 4.419 -1.371 -4.366 -4.948 -4.    937 -1.374 -4.377 -4.370 -4.943 -1.389 4.955 -1.368 -4.376 1.396 -4.364 -4.947 4.394 -4.939 -1.388 4.953 1.400 -4.369 1.390 4.395 -4.927     4.387 4.955 -1.365 -1.377 4.393 -4.928 -4.377 4.949 -1.366 -4.939 1.394 -4.370 5.840 -0.000 2.216 8.057 -0.000 3.044
Stage 1 mv .bs and .mv files to .json. Easiest way to accomplish this is to make a python script that will translate the .bs file into a .json files. Depending on how your atomistic data is stored these parsers can be adapted accordingly. The end point is an xbs-bs.json.