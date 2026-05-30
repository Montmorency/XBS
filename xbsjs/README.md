Michael Methfessel's XBS viewer allows you to create visual atomistic models
that have a retro style feel to them. For example:

In this notebook I have brought the original XBS library into the observablehq framework. This is basically a C to JS port but with the added benefits of the observable notebook. We want to capture some of the feel of X11 using svg/canvas while excising all the C/X11 dependencies in favour of browser friendly tools.

XBS-Observable
The result of the port is below. The viewer supports wheel based zooming for scaling and an implementation of versor dragging appropriate for atomistic systems. If a keypad is available additional controls can be used to advance frames and toggle perspective, bond linestyle, and atom fill. The philosophy is this observablehq/js implementation lets you upload your atomistic data and conveniently inspect and reorient your system in a browser. You can also embed your images in your own notebooks or webpages. If you wish to fine tune the diagram you can manipulate the svg using d3 tools, or perhaps more conveniently, export the svg file and perform your manipulations by hand in your favourite vector graphics editor. The atoms have id attributes corresponding to their label and number so you can group selections on them and resize/recolour etc.

Gesture Commands
   left arrow: rotate left
   right arrow: rotate right
   ' : rotate up
   / : rotate down
   < : rotate counterclockwise
   > : rotate clockwise
   p : toggle perspective
   l : toggle linestyle
   w : wire frames
   [ : frame left (film)
   ] : frame right (film)
   r : reset to home view
   j : first frame
   k : last frame


The observablehq framework makes it really convenient to save/export your molecular image as a png/svg for further postprocessing. You can just click in the left margin and choose Download SVG/PNG.

To swap in your own system you can upload your own data (Shift-Command-U) in the accepted xbs.json format (described below with a python conversion script).


Some Notes on XBS
A typical XBS session loads a '.bs' file (possibly with a .mv file in same directory if we are viewing an animation). An X11 session, with the help feature activated, looks like this:

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
bs_kernel (Important! sort atoms back to front, make list of sphere centers and radii, start loop over atoms plot ball first DrawBall, make list of bonds to Gatom, inner loop over bonds, plots sticks DrawStick, writes bond lengths if desired.
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

An Input File looks like:
   ...







