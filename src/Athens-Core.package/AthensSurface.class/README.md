I representing a surface, where all drawing operations will happen.

I also having a number of factory methods to create paints, paths and other objects involved in drawing.

AthensSurface is an abstract, while subclasses implement a specific kind of surface for one or another backend.

The primary role of AthensSurface class is to define a public protocol for all Athens surfaces, which can be used by applications which using Athens framework.

To get a new surface, use:

<one of my subclasses> extent: x@y

for surfaces which don't need to have dimensions specified,
it would just #new.