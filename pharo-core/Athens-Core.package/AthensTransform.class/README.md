My instances represent an interface for coordinate system transformation defined by 3x3 matrix of following kind:

| sx   shx   x   |
| shy sy     y   |
|  w0  w1  w2 |

sx and sy define scaling in the x and y directions, respectively;
shx and shy define shearing in the x and y directions, respectively;
tx and ty define translation in the x and y directions, respectively.

AthensSurface are responsible for providing specific implementation 
of me, which is most appropriate for backend its using.

My internal representation isnt available for manipulation, for example dont assume i'm a matrix. In short, do not copy my state or modify it directly, instead use methods such as #loadAffineTransformation:, and talk to my surface.

My subclasses should implement a common protocol for applying generic types of affine transformations
on coordinate system:
- translate
- rotate
- shear
- scale
- matrix multiply
- matrix load

Surface must support 4 kinds of transformations of coordinate system(s):
- path-to-surface (#pathTransform) to transform path coordinates to surface space
- image-to-surface (#imageTransform) to transform between user coordinates and surface pixels

- fill-paint-to-user (#fillTransform)
- stroke-paint-to-user (#strokeTransform)

Given a (fill or stroke) paint-to-user transformation Tp and user-to-surface transformation Tu, the paint color and alpha of a pixel to be drawn with surface coordinates (x, y) is defined by mapping its center point (x + 1/2, y + 1/2) through the inverse transformation (Tu * Tp)^-1 , resulting in a sample point in the paint coordinate space.

All transformations, except image-to-surface , ignoring w0 , w1 and w2 values and always assume them set to { 0 , 0 , 1 } respectively.

For accessing a particular kind of transformation, send message to canvas, i.e.:

canvas pathTransform translateBy: 10@10.

It is safe to store transformation in temporary variable, as long as canvas is valid.
