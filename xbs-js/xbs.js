
consts NPOINTS = 5;
height = 450
width = 450
left_margin = 20
right_margin = 20
top_margin = 20
bottom_margin = 20
dalfa = 0.08726646259971647
bndfac = 1
NPOINTS = 5
d = Array(3) [0, 0, 250]
MAXRAD = 100


/*
https://observablehq.com/@d3/versor-dragging

The result of the port is below. The viewer supports wheel based zooming for scaling and an implementation of versor dragging appropriate for atomistic systems. If a keypad is available additional controls can be used to advance frames and toggle perspective, bond linestyle, and atom fill. The philosophy is this observablehq/js implementation lets you upload your atomistic data and conveniently inspect and reorient your system in a browser. You can also embed your images in your own notebooks or webpages. If you wish to fine tune the diagram you can manipulate the svg using d3 tools, or perhaps more conveniently, export the svg file and perform your manipulations by hand in your favourite vector graphics editor. The atoms have id attributes corresponding to their label and number so you can group selections on them and resize/recolour etc.

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

*/

viewof xbs = { 
    var xbs = DOM.svg(width, height)
    var svg = d3.select(xbs)
                  .attr("class","xbs")
                  .attr("width", width)
                  .attr("height", height)   
                  .attr("font-family", "sans-serif")
                  .attr("font-size", 10)
                  .attr("focusable","true")
                  .attr("tabindex","-1")
                  .style("display", "block")
  svg.attr('style', 'border: 1px solid gray')
  xbs.value = xbs 
  return xbs;
}

d3.select(xbs)
  .call(zoom(projection));

d3.select(xbs)
  .call(zoom(projection));

data = FileAttachment("ringmv@1.json").json()

xscale =  {
           return d3.scaleLinear()
                       .domain([d3.min(data.atoms.coords, d => atompos_perm(d,1)[0]), 
                                d3.max(data.atoms.coords, d =>  atompos_perm(d,1)[0])]
                                      )
                       .range([0+left_margin, width-right_margin])
                       .interpolate(d3.interpolateRound);
}

yscale =  {
           return d3.scaleLinear()
              .domain([d3.min(data.atoms.coords, d => atompos_perm(d,1)[1]), 
                              d3.max(data.atoms.coords, d =>  atompos_perm(d,1)[1])]
                                      )
              .range([0+bottom_margin, height-top_margin])
              .interpolate(d3.interpolateRound);
}


draw_sticks = {
  // NB atompos(pcoords[])[] pcoords takes place of Methfessel's zp[], zr structs
  // this handles mapping to page coordinates via d3.
            var svg = d3.select(xbs)
            tmat; // make cell dependent on tmat
            perspective;
            dist0;
            bline;
            scale;
            
  // constants
            const natom = pcoords.length;
            const nbond = stick.length;
            const fudgefac=0.6;
            const gslope = 0;
            const gz0 = 0;
            
  // don't think we need these as d3/svg takes care of it:
            const taux = 0.0; //width/2.0;
            const tauy = 0.0; //height/2.0;
            
  // arrays
            var ip = Array(natom);
            var kbx = Array(16); 
            var abx = Array(16);
            var pbx = Array(16);
            var fbx = Array(16);
            var q1 = Array(3);
            var q2 = Array(3);
            var b = Array(3);
            var m1 = Array(6);
            var m2 = Array(6);       
            var bxy = Array(3);
            var bond_list = [];
  // counters
            var k = 0;
            var kk = 0;
            var nbx = 0;
            var ibx = 0;
            var jbx = 0;
            var ib = 0;
            var note = 0;
            var note1 = 0;
            var note2 = 0;
            var bx,by,br,bxy,xx,cth1,cth2,th1,th2;
            var rk, rkk;
            var crit1,crit2;
            var w, sth1, ww, bb, aa;
            var sth2,ww;
            var beta;
            var midx, midy;
            var x1,y1,x2,y2;
            var x,y;
  
            midx = 0.0; // width/2.0;
            midy = 0.0; //height/2.0;
            const max_bond = 16; 
            var p_loc = pcoords.concat();
            
  // get sorted atom indices `back to front' i.e. along z in page.
            for (let i=0; i<natom; i++) {
              ip[i] = i;
            }            
            var ip_coords = ip.map(function(e, i) {
                  return [e, p_loc[i]];
              });
            var balls_sticks = Array(natom);            
            for (let i=0; i< natom; i++){
              balls_sticks[i] = [pcoords[i].concat(), Array()];
            }
            
            for (let n = 0; n < natom; n++) {
              // k is an index
              k = ip_coords[n][0];  
              rk = ball[k].rad;
              
/*  ------ make list of bonds to this atom ----- */              
              nbx = 0;              
              for (let j=0; j < nbond; j++) {
                if(k == stick[j].start) {
                   kbx[nbx] = j;
                   abx[nbx] = stick[j].end;
                   nbx++;
                }
                else if (k == stick[j].end) {
                  kbx[nbx] = j;
                  abx[nbx] = stick[j].start;
                  nbx++;
                  };
                };
                
              if (nbx==0) { continue; };
              
              for (let m=0; m<nbx; m++){
                fbx[m]=0;
              };
              
              // find `bottom' atom of bondlist
              for (let m=0; m<nbx; m++) {
                  var bot = 1.e10;
                  var ibot = 0;
                
                  for (let j=0; j < nbx; j++) {
                    if (ip_coords[abx[j]][1][2] < bot && !fbx[j]) {
                       bot = ip_coords[abx[j]][1][2];
                       ibot=j;
                       };
                  }
                    pbx[m] = ibot;
                    fbx[ibot]=1;
                }

/*  ------ inner loop over bonds ----- */

              for (let ibx=0; ibx < nbx; ibx++) {
                  jbx = pbx[ibx];
                  kk = abx[jbx];
                  ib = kbx[jbx];
                  
                  //if (ib<0) printf("this cannot happen\n");
                  
                  rkk = ball[kk].rad; 
                
                  if (ib >= 0) {
                    br = bndfac*stick[ib].rad;
                    vsum(atompos(ip_coords[kk][1], rkk), atompos(ip_coords[k][1], rk),1.0,-1.0, bxy);
                    
                    bx = bxy[0];
                    by = bxy[1];
                    xx = Math.sqrt(bx*bx +by*by);
                    if (xx*xx < 0.0001) continue;
                    
                  // Find projection of bond?
                    bx = bx/xx;
                    by = by/xx;
                    
                    vsum(d, ip_coords[k][1],  1.0, -1.0, q1);
                    vsum(d, ip_coords[kk][1], 1.0, -1.0, q2);
                    vsum(ip_coords[kk][1], ip_coords[k][1], 1.0, -1.0, b);
                    
                    cth1 =  sp(q1,b) / Math.sqrt(sp(q1,q1)*sp(b,b));
                    th1=Math.acos(cth1);
                    
                    cth2 = -sp(q2,b) / Math.sqrt(sp(q2,q2)*sp(b,b));                    
                    th2=Math.acos(cth2);
                    
                    crit1 = Math.asin(br/rk) * fudgefac;
                    if (crit1<0.0) crit1=0.0;
                    
                    crit2 = Math.asin(br/rkk) * fudgefac;
                    if (crit2<0.0) crit2=0.0;
                    
                    note = 0;
                    if((th2-0.5*Math.PI > crit2) && (k<kk)) note=1;
                    if((th1-0.5*Math.PI < crit1) && (k>kk)) note=2;

                    /* ------- plot a stick ------ */
                    
                    if (note == 1 || note==2) {  
                      
                      w = Math.sqrt(rk*rk - br*br);
                      sth1 = Math.sqrt(1.0-cth1*cth1);
                      
                      ww = w*sth1*atompos(ip_coords[k][1],rk)[2]/rk;
                      bb = br*atompos(ip_coords[k][1],rk)[2]/rk;
                      aa = br*cth1*atompos(ip_coords[k][1],rk)[2]/rk;
                      
                      m1[0] = bx*aa;   
                      m1[1] = by*aa;
                      m1[2] =-by*bb;  
                      m1[3] = bx*bb;
                      m1[4] = atompos(ip_coords[k][1],rk)[0] + bx*ww + taux;
                      m1[5] = atompos(ip_coords[k][1],rk)[1] + by*ww + tauy;
                      
                      w = Math.sqrt(rkk*rkk-br*br);
                      sth2 = Math.sqrt(1.0-cth2*cth2);
                      ww = w*sth2*atompos(ip_coords[kk][1],rkk)[2]/rkk;
                      bb = br*atompos(ip_coords[kk][1],rkk)[2]/rkk;
                      aa = br*cth2*atompos(ip_coords[kk][1],rkk)[2]/rkk;
                                          
                      m2[0] = bx*aa;   
                      m2[1] = by*aa;
                      m2[2] = -by*bb;  
                      m2[3] = bx*bb;
                      m2[4] = atompos(ip_coords[kk][1], rkk)[0] - bx*ww + taux;
                      m2[5] = atompos(ip_coords[kk][1], rkk)[1] - by*ww + tauy;
                      
                      beta = Math.exp(gslope*(0.5*(ip_coords[k][1][2]+ip_coords[kk][1][2])-gz0)*gslope);
                    
                    // plot stick
                    
                      if (bline){
                        x1 = m1[4];
                        y1 = m1[5];
                        
                        x2 = m2[4];
                        y2 = m2[5];
                        
                        balls_sticks[k][1].push (
                          { x1:xscale(x1), y1:yscale(y1)
                          , x2:xscale(x2), y2:yscale(y2)
                          }
                        ) 
                      }
                        
                    // thick bond with narrowing towards vanishing point 
                      else if(!bline){
                          var pp = Array(NPOINTS*2+1);
                          for (let i=0; i<NPOINTS; i++) {
                                x=m1[0]*arc[i][0]+m1[2]*arc[i][1]+m1[4];
                                y=m1[1]*arc[i][0]+m1[3]*arc[i][1]+m1[5];
                                pp[i]= {x:x,y:y};
                              }
                          for (let i=0;i<NPOINTS;i++) {
                                x=-m2[0]*arc[i][0]+m2[2]*arc[i][1]+m2[4];
                                y=-m2[1]*arc[i][0]+m2[3]*arc[i][1]+m2[5];
                                pp[2*NPOINTS-i-1] = {x:x,y:y};
                            }
                          pp[2*NPOINTS]=pp[0];
                          balls_sticks[k][1].push(pp.concat());
                      }
                  } // note
               } //ib
             } //ibx
         } //k /n
         draw_balls_3(balls_sticks);
}

function draw_balls_3(balls_sticks) {
          // balls_stick  [[[x,y,z], [bonds]]] <- see how overloaded lists are here... could
          // use an object i suppose?
          var svg = d3.select(xbs)
          var pos;
          var bonds;
          var natom = balls_sticks.length;
          tmat; //make cell dependent on changes in tmat.
          dist0;
          bline;
          scale;
  
      var flag = Array(natom);
      var ip = Array(natom);
  
    //Just stick with Methfessels way don't try to do anything stupid.
    
      for(let k=0; k<natom; k++){ 
        flag[k]=0;
      }
  
      for(let n=0; n<natom; n++) {
         let bot = 1.0e10; 
         let ibot = 0;
         
         for (let k=0; k < natom; k++) {
           if (pcoords[k][2] < bot && !flag[k]) 
               { bot = pcoords[k][2]; 
                 ibot = k; 
               }
           }
         ip[n]=ibot;
         flag[ibot]=1;
        }
        
     // scrub paths and circles everytime <- this is inefficient?
          svg.selectAll("circle").remove();
          svg.selectAll("path").remove();
          
          for (let k  = 0; k < natom; k++) {
            let i = ip[k];
            pos = balls_sticks[i][0].concat();
            
            svg.append("circle")
                 .attr("id",`${ball[i].spec}${i}`)
                 .attr("cx", xscale(atompos(pos, ball[i].rad)[0]))
                 .attr("cy", yscale(atompos(pos, ball[i].rad)[1]))
                 .attr("r",2.0*atompos(pos, ball[i].rad)[2]) 
                 .style("fill", wire_fill(ball[i].colour))
                 .style("stroke", "black");
          
            bonds = balls_sticks[i][1].concat();
            
              if(bline){
                bonds.forEach(d => {
                  svg.append("path")
                     .style("stroke", "black")
                     .style("fill", "black")
                     .attr("d", d3.line()([[d.x1,d.y1],[d.x2,d.y2]]));
                })
              }
              else if(!bline){
                bonds.forEach(d=> {
                svg.append("path")
                   .style("stroke", "black")
                   .style("fill", "black")
                   .attr("d", line(d));
                })
               }
         } // natoms
}

// Take the global atomic coordinates and rotate them according to tmat.
pcoords = {
  frame_index;
  let natom = data.atoms.coords.length;
  
  let p = Array(natom);
  for(let n=0; n<natom;n++){
    p[n] = Array(3);
  }
  
  for(let n =0; n<natom; n++){
    for (let m=0; m<3; m++){
       p[n][m] = tmat[m][0]*glob_coords[n][0] +
                 tmat[m][1]*glob_coords[n][1] +
                 tmat[m][2]*glob_coords[n][2];
      }
    }
    return p;
}

ball = {
       //adding species names can select in svg on this attribute
        const natoms = data.atoms.species.length;
        const nspecies = data.species.length;
        
        var ball = Array(natoms);
        
        var spec;
        
        for (let i=0;i<natoms; i++){
          spec = data.atoms.species[i];
          for (let j =0; j<nspecies;j++){
            if (spec == data.species[j].name)
           {
             ball[i] = { rad:parseFloat(data.species[j].r)
                       , spec:spec
                       , colour:data.species[j].colour
                       };
               }
           }
       }
       return ball;
}

stick = {
  //build bonds
  const nbondtypes = data.bonds.length
  const nbas = data.atoms.coords.length
  
  let max_bonds = nbas*16 //maximum 16 bonds per atom.
  var stick = [];
  
  let i = 0;
  let kb = 0;
  let dis = 0.0;
  let dd = 0.0;
  
  i=-1
  for(let k=0; k<nbas; k++){
    for(let l=k+1; l<nbas; l++){
      kb = -1
      for (let j=0; j < nbondtypes; j++) {
        if (match(data.atoms.species[k], data.bonds[j].name1) && 
            match(data.atoms.species[l], data.bonds[j].name2)){kb=j};
            
        if (match(data.atoms.species[l], data.bonds[j].name1) && 
            match(data.atoms.species[k], data.bonds[j].name2)){kb=j};
      }
      
      if (kb>-1) {
      dis=0.0;
      for (let m=0; m < 3; m++) {
        dd = glob_coords[k][m]-glob_coords[l][m];
        dis = dis+dd*dd;
      }
      
      dis = Math.sqrt(dis);
        if ((dis>= data.bonds[kb].min_length) && (dis<=data.bonds[kb].max_length)){
             i++;
             stick.push({start:k,
                         end:l,
                         rad:parseFloat(data.bonds[kb].radius),
                         //gray:parseFloat(data.bonds[kb].gray),
                         col:parseFloat(data.bonds[kb].colour)});  
        }
      }
    }
  }
  return stick;
}


function wire_fill(ball_colour){
    // Expecting a ball colour on scale [0,1] which gets converted to gray scale.
                     if  (wire) {return "none";}
                     let gscale = ball_colour*256;
                     if (!wire) {return `rgb(${gscale}, ${gscale}, ${gscale})`;}
}

function atompos_perm(loc_p, rad){
  //pmode=1no perspective to set default scales
  let zp = Array(3);
  let zr = 0;
  //if(pmode == 1){
    zp[0] = 15*loc_p[0];
    zp[1] = 15*loc_p[1];
    zr = 15*rad;
    zr = MAXRAD;
    if((dist0-loc_p[2]) > 0) {zr = (15*rad*dist0)/(dist0-loc_p[2])};
    if(zr > MAXRAD) {zr = MAXRAD};
    zp[2] = zr;
    return zp;
 // }
}

function atompos(loc_p, rad){
  //pmode=1no perspective
  let zp = Array(3);
  let zr = 0;
  
  var a,b;
  var xxx;
  var za1,za2,zb1,zb2;
  
  var v1 = Array(3);
  var v2 = Array(3);
  var q = Array(3);
  var y = Array(3);
  
  if(perspective == 1){
    zp[0] = scale*loc_p[0];
    zp[1] = scale*loc_p[1];
    zr = scale*rad;
    zr = MAXRAD;
    
    if((dist0-loc_p[2]) > 0) {zr = (scale*rad*dist0)/(dist0-loc_p[2])};
    if(zr > MAXRAD) {zr = MAXRAD};
    
    zp[2] = zr;
    return zp;
  }
  
   vscal(loc_p, 1.0, q);
   q[2] = q[2]-dist0;
   vscal(loc_p, 1.0, y);
   xxx = -sp(y,q)/sp(q,q);
   vsum(y, q, 1.0, xxx, y);
   
   if(sp(y,y)<=1e-3){ 
     y[0]=1.0; 
     y[1]=0.0; 
     y[2]=0.0; 
   }
    
    a = -rad*rad / sp(q,q);
    b = rad*Math.sqrt((1.0+a) / sp(y,y));
    vsum(q, y, a, b, v1);
    vsum(q, y, a, -b, v2);
    vsum(loc_p, v1, 1.0, 1.0, v1);
    vsum(loc_p, v2, 1.0, 1.0, v2);
    
    za1 = scale*v1[0]*dist0 / (dist0-v1[2]);
    za2 = scale*v1[1]*dist0 / (dist0-v1[2]);
    zb1 = scale*v2[0]*dist0 / (dist0-v2[2]);
    zb2 = scale*v2[1]*dist0 / (dist0-v2[2]);
    zp[0] = 0.5*(za1+zb1);
    zp[1] = 0.5*(za2+zb2);
    zr = (zb1-za1)*(zb1-za1) + (zb2-za2)*(zb2-za2);
    zr = 0.5 * Math.sqrt(zr);
    zp[2] = zr;
  return zp;
}

arc = {
  var arc = Array(NPOINTS);
  var phi=0.0;
  for (let i=0;i<NPOINTS;i++){
    arc[i] = Array(2);
  };
  for (let i=0;i<NPOINTS;i++){
    phi = i*3.1415926/(NPOINTS-1.0);
    arc[i][0] = -Math.sin(phi);
    arc[i][1] = Math.cos(phi);
  };
       
  return arc;
  }

function line(d){
  //take list of arc points to draw thick bonds
  //
  var lop = []
  for(let i=0;i<d.length;i++)
    {lop.push([xscale(d[i].x), yscale(d[i].y)]);
   }
  return d3.line()(lop);
}

function match(str,pat) {return str == pat;}

function advance_frame(n){
  //update data.frame add(or subtract frame num) and mod out length
  var natom = data.atoms.coords.length;
  //this is the number of frames list of coords:
  var nframes = data.frames.coords.length;
  
  if (data.frames.coords.length ==0){return ;}
  
  var tmp_index = frame_index + n;
  //console.log(nframes,natom);
  if (tmp_index<0){tmp_index += nframes+1;}
  viewof frame_index.value = tmp_index % (nframes);
    
  let p = Array(natom);
  for(let n=0; n<natom;n++){
    p[n] = Array(3);
  }
  
  //updates glob_coords
  viewof glob_coords.value = data.frames.coords[frame_index];
  return;
}

key_pad = { const svg = d3.select(xbs)
//           svg.on('keydown', function() {
//                   const [x, y] = d3.mouse(this);
//                   svg.append('circle').attr('r', 5).attr('cx', x).attr('cy', y)
           svg.on("keydown", function(){
                  switch(d3.event.keyCode)
                     {  case 39:  //key_right
                        rotmat(1, dalfa)
                        break;
                        case 37:  //key_left
                        rotmat(1, -dalfa)
                        break;
                        case 222: //apostrophe
                         rotmat(2,dalfa);
                         break;
                       case 191:
                         rotmat(2,-dalfa);
                         break;
                       case 188:
                         rotmat(3,-dalfa);
                         break;
                       case 190:
                         rotmat(3,dalfa);
                         break;
                       case 87:
                         //w: wire
                         toggle_wire();
                         break;
                       case 80:
                         //p:perspective
                         toggle_perspective();
                         break;
                       case 76:
                         //l: line bonds (bline)
                         toggle_bline();
                         break;
                       case 187:
                         //plus:zoom in
                         viewof scale.value *= 1.05;
                         break;
                       case 189:
                         //plus:zoom in
                         viewof scale.value /=1.05;
                         break;
                       case 219:
                         //bracket left
                        advance_frame(-1);
                        break;
                       case 221:
                         //bracket_right
                         advance_frame(1);
                         break
                       case 74:
                       //switching to j from {
                         viewof frame_index.value = 0;
                         viewof glob_coords.value = data.frames.coords[frame_index]
                       break;
                       //switching to k from }
                       case 75:
                         viewof frame_index.value = data.frames.coords.length-1;
                         viewof glob_coords.value = data.frames.coords[frame_index]
                         break;
                       case 82:
                         //r reset to home view
                         viewof tmat.value = [[1,0,0],[0,1,0],[0,0,1]];
                         break;
                       
                      }
                   }
                 );
  };

class Projection {
  //Azimuthal Projection
  project() {return ;}
    
  invert(coord) {
    var x,y;
    x = coord[0];
    y = coord[1];
    //make sphere cover projected 2D area
    let R = Math.sqrt(width*width + height*height);
    var z = Math.sqrt(x * x + y * y),
        c = asin(z/R),
        sc = Math.sin(c),
        cc = Math.cos(c);
    //console.log(z,c,sc,cc)
    return [
      Math.atan2(x * sc, z * cc),
      asin(z && y * sc / z)
    ];
  }
  
  rotate(x){
    return arguments.length ? [x[0],x[1],0]: [0,0,0];
  }
}

function rotmat(ixyz, alfa){
  let i = 0;
  let j = 0; 
  let k =  0;
  let rot = Array(3);
  let w = Array(3);
  
  for(let i =0; i <3; i++){
    rot[i] = Array(3);
    w[i] = Array(3);
  }
  
  switch (ixyz) {
    case 3:
      rot[0][0]=Math.cos(alfa);  
      rot[0][1]=-Math.sin(alfa); 
      rot[0][2]=0.0;
      rot[1][0]=Math.sin(alfa);  
      rot[1][1]=Math.cos(alfa);  
      rot[1][2]=0.0;
      rot[2][0]=0.0;        
      rot[2][1]=0.0;        
      rot[2][2]=1.0;
      break;
    case 1:
      rot[0][0]=Math.cos(alfa);  
      rot[0][1]=0.0;        
      rot[0][2]=Math.sin(alfa);
      rot[1][0]=0.0;        
      rot[1][1]=1.0;        
      rot[1][2]=0.0;
      rot[2][0]=-Math.sin(alfa); 
      rot[2][1]=0.0;        
      rot[2][2]=Math.cos(alfa);
      break;
    case 2:
      rot[0][0]=1.0;        
      rot[0][1]=0.0;        
      rot[0][2]=0.0;
      rot[1][0]=0.0;        
      rot[1][1]=Math.cos(alfa);  
      rot[1][2]=-Math.sin(alfa);
      rot[2][0]=0.0;        
      rot[2][1]=Math.sin(alfa);  
      rot[2][2]=Math.cos(alfa);
      break;
    default:
      return;
    }
  
  for(let i=0;i<3;i++){ 
    for(let j=0;j<3;j++){
      w[i][j]=0.0;
      for(let k=0;k<3;k++){
        w[i][j] = w[i][j] + rot[i][k]*tmat[k][j];
      }
    }
  }

    for(let i=0;i<3;i++){ 
      for(let j=0;j<3;j++) {
          tmat[i][j] = w[i][j];
      }
    }
  viewof tmat.value = tmat;
}

function eumat(alfa=0.0,beta=0.0,gama=0.0){
  let gam = new Array(3);
  let bet = new Array(3);
  let alf = new Array(3);
  let w = new Array(3);
  let tmat1 = new Array(3);
  
  for (let i=0;i<3;i++){
    gam[i] = new Array(3);
    bet[i] = new Array(3);
    alf[i] = new Array(3);
    tmat1[i] = new Array(3);
    w[i] = new Array(3);
  };
  
  gam[0][0]=1.0;        
  gam[0][1]=0.0;        
  gam[0][2]=0.0;
  gam[1][0]=0.0;        
  gam[1][1]=Math.cos(gama);  
  gam[1][2]=-Math.sin(gama);
  gam[2][0]=0.0;        
  gam[2][1]=Math.sin(gama);  
  gam[2][2]=1.0;
  
  bet[0][0]=Math.cos(beta);  
  bet[0][1]=0.0;        
  bet[0][2]=Math.sin(beta);
  bet[1][0]=0.0;        
  bet[1][1]=1.0;        
  bet[1][2]=0.0;
  bet[2][0]=-Math.sin(beta); 
  bet[2][1]=0.0;        
  bet[2][2]=Math.cos(beta);
  
  alf[0][0]=Math.cos(alfa);  
  alf[0][1]=-Math.sin(alfa); 
  alf[0][2]=0.0;
  alf[1][0]=Math.sin(alfa);  
  alf[1][1]=Math.cos(alfa);  
  alf[1][2]=0.0;
  alf[2][0]=0.0;        
  alf[2][1]=0.0;        
  alf[2][2]=1.0;
  
  for(let i=0; i<3; i++) {
    for(let j=0; j<3; j++) {
      w[i][j]=0.0;
      for(let k=0;k<3;k++) {w[i][j]=w[i][j] + bet[i][k]*alf[k][j]};
    };
  };
  
  for(let i=0; i<3; i++) {
    for(let j=0; j<3; j++) {
      tmat1[i][j]=0.0;
      for(let k=0;k<3;k++){
        tmat1[i][j] = tmat1[i][j] + gam[i][k]*w[k][j]
      };
    };
  };
  
  return tmat1;
}

function zoom(projection) {
  let v0, q0, r0;
  function dragstarted() {
  //adapted from Bostock
  //my projection function is not a geoprojection
  //https://observablehq.com/@d3/versor-dragging
    v0 = versor.cartesian(projection.invert(d3.mouse(this)));
    r0 = [0,0,0];
    q0 = versor(r0);
  }
  
  function dragged() {
    viewof scale.value = d3.event.transform.k
    var v1 = versor.cartesian(projection.invert(d3.mouse(this))),
        q1 = versor.multiply(q0, versor.delta(v0, v1));
        let eu_angles = versor.rotation(q1);
        projection.rotate(eu_angles);
   
    rotmat(1,eu_angles[0]);
    rotmat(2,eu_angles[1]);
    rotmat(3,eu_angles[2]);
  }
  function dragended(){
    return;
  }
  return d3.zoom()
      .on("start", dragstarted)
      .on("zoom", dragged)
      .on("end", dragended);
}

function vsum(a,b,ca,cb,v)
  {
    v[0]=ca*a[0]+cb*b[0];
    v[1]=ca*a[1]+cb*b[1];
    v[2]=ca*a[2]+cb*b[2];
    return;
  }

function sp(a, b)
//scalar product
  {
    var sp1;
    sp1=a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
    return sp1;
  }


/* ----- dbond ------- */
function dbond(gray,  m1, m2)
  {
    const dfac=0.8;
    const rfac=2.0;

    var r,ax,ay,bx,by,a1,b1,dx,dy,alf,d1,bb,x;
    var m1 = Array(6);
    var m2 = Array(6);

    m2[4]=dfac*m2[4]+(1-dfac)*m1[4];
    m2[5]=dfac*m2[5]+(1-dfac)*m1[5];

    ax=m2[0];
    ay=m2[1];
    bx=m2[2];
    by=m2[3];

    b1=Math.sqrt(bx*bx+by*by);
    a1=Math.sqrt(ax*ax+ay*ay);
    
    r=rfac*b1;
    
    m2[0] = -r*ax/a1;
    m2[1] = -r*ay/a1;
    m2[2] = r*bx/b1;
    m2[3] = r*by/b1;

    dx=m2[4]-m1[4];
    dy=m2[5]-m1[5];

    if(m2[0]*dx<0 || m2[1]*dy<0) {
      m1[0]=-m1[0]; m1[1]=-m1[1]; m1[2]=-m1[2]; m1[3]=-m1[3];
      m2[0]=-m2[0]; m2[1]=-m2[1]; m2[2]=-m2[2]; m2[3]=-m2[3];
    }
    
    d1=Math.sqrt(dx*dx+dy*dy);
    
    bb=Math.sqrt(m1[2]*m1[2]+m1[3]*m1[3]);
    
    if(r-bb<d1) {
      x=bb*d1/(r-bb);
      alf=Math.asin(bb/x)*57.3;
      }
  }

function haversin(x) {
  return (x = Math.sin(x / 2)) * x;
}

function asin(x) {
  return x > 1 ? 0.5*Math.PI : x < -1 ? -0.5*Math.PI : Math.asin(x);
}

function acos(x) {
return x > 1 ? 0 : x < -1 ? Math.PI : Math.acos(x);
}

function eumat(alfa=0.0,beta=0.0,gama=0.0){
  let gam = new Array(3);
  let bet = new Array(3);
  let alf = new Array(3);
  let w = new Array(3);
  let tmat1 = new Array(3);
  
  for (let i=0;i<3;i++){
    gam[i] = new Array(3);
    bet[i] = new Array(3);
    alf[i] = new Array(3);
    tmat1[i] = new Array(3);
    w[i] = new Array(3);
  };
  
  gam[0][0]=1.0;        
  gam[0][1]=0.0;        
  gam[0][2]=0.0;
  gam[1][0]=0.0;        
  gam[1][1]=Math.cos(gama);  
  gam[1][2]=-Math.sin(gama);
  gam[2][0]=0.0;        
  gam[2][1]=Math.sin(gama);  
  gam[2][2]=1.0;
  
  bet[0][0]=Math.cos(beta);  
  bet[0][1]=0.0;        
  bet[0][2]=Math.sin(beta);
  bet[1][0]=0.0;        
  bet[1][1]=1.0;        
  bet[1][2]=0.0;
  bet[2][0]=-Math.sin(beta); 
  bet[2][1]=0.0;        
  bet[2][2]=Math.cos(beta);
  
  alf[0][0]=Math.cos(alfa);  
  alf[0][1]=-Math.sin(alfa); 
  alf[0][2]=0.0;
  alf[1][0]=Math.sin(alfa);  
  alf[1][1]=Math.cos(alfa);  
  alf[1][2]=0.0;
  alf[2][0]=0.0;        
  alf[2][1]=0.0;        
  alf[2][2]=1.0;
  
  for(let i=0; i<3; i++) {
    for(let j=0; j<3; j++) {
      w[i][j]=0.0;
      for(let k=0;k<3;k++) {w[i][j]=w[i][j] + bet[i][k]*alf[k][j]};
    };
  };
  
  for(let i=0; i<3; i++) {
    for(let j=0; j<3; j++) {
      tmat1[i][j]=0.0;
      for(let k=0;k<3;k++){
        tmat1[i][j] = tmat1[i][j] + gam[i][k]*w[k][j]
      };
    };
  };
  
  return tmat1;
}

function zoom(projection) {
  let v0, q0, r0;
  function dragstarted() {
  //adapted from Bostock
  //my projection function is not a geoprojection
  //https://observablehq.com/@d3/versor-dragging
    v0 = versor.cartesian(projection.invert(d3.mouse(this)));
    r0 = [0,0,0];
    q0 = versor(r0);
  }
  
  function dragged() {
    viewof scale.value = d3.event.transform.k
    var v1 = versor.cartesian(projection.invert(d3.mouse(this))),
        q1 = versor.multiply(q0, versor.delta(v0, v1));
        let eu_angles = versor.rotation(q1);
        projection.rotate(eu_angles);
   
    rotmat(1,eu_angles[0]);
    rotmat(2,eu_angles[1]);
    rotmat(3,eu_angles[2]);
  }
  function dragended(){
    return;
  }
  return d3.zoom()
      .on("start", dragstarted)
      .on("zoom", dragged)
      .on("end", dragended);
}
