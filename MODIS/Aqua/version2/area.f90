
C Area.f         Return the area of a polygon
C
C INPUTS:  Array X with the X coords, and Y with the Y coords.
C          N: Number of verts.
C OUT:     The area.
C
C If there is only one connected component, the first vertex must be
C repeated as the last vertex.
C
C The polygon can have multiple separate components, possibly nested,
C provided that each component has its first vertex repeated as the last
C vertex of that component.  The vertices of each outside component must
C be listed in positive order, those of any immediately contained
C component in negative order, etc.
C
C Wm. Randolph Franklin,  wrfATecse.rpi.edu, (518) 276-6077;  Fax: -6261
C ECSE Dept., 6026 JEC, Rensselaer Polytechnic Inst, Troy NY, 12180 USA
C
C Last modified: Fri Sep 30 14:33:18 1994
C
C This is modelled on a function that I wrote about 1971.
C
C
      FUNCTION AREA(X,Y,N)
      DIMENSION X(1), Y(1)

      AREA=0.0
      IF (N-3) 1,1,2
 1    WRITE(2,3) N
 3    FORMAT(19HUnusual value of N=,I10,17H in call to AREA.)
      RETURN

 2    IF(N-1000000) 7,1,1
 7    DO4I=3,N,2
 4    AREA=AREA+(X(I-2)-X(I))*Y(I-1)-(Y(I-2)-Y(I))*X(I-1)

      IF (MOD(N,2)) 6,5,6
 5    AREA=AREA+X(N-1)*Y(N)-Y(N-1)*X(N)

 6    AREA=AREA*0.5
      RETURN
      END
