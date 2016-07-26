I am a generic meta class for creating a subclasses, which instances will hold a single value of specified C type.
To create a new class for some concrete C type you need to declare an anonnymous class and indicate the type you want to use:

[ [ [ 
floatHolderClass := FFIExternalValueHolder ofType: 'float'
 ] ] ]

Then you can use  given class(es) as a value holders for type you specified:

[ [ [ 
 float := floatHolderClass new.

float value:  1.5
float value
 ] ] ]

By combining this with class/pool variables we have a convenient way of defining a values, which are passed by pointer to a function.

For instance, imagine that we need to create a binding to a function:

[ [ [ 
void getFoo( SomeType * value) 
 ] ] ]

which is not an unusual situation, when C function using pointer arguments for storing it's output there.

And this is what ==FFIExternalValueHolder== is done for. To define a binding to this function you can declare a subclass named SomeType
 
and then use it in function signature:

[ [ [ 
getFoo: value
  ^ self ffiCall: #(void getFoo ( SomeType *  value ))
] ] ]

and call it like: 

[ [ [ 
var := SomeType new.
self getFoo: var.

var value  "will read the value" 
]]]