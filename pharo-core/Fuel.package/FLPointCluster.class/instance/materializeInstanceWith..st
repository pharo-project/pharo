serialize/materialize
materializeInstanceWith: aDecoder
	"Since in Pharo #@ is associated with a special bytecode, it is faster than doing  Point basicNew"
	^ 0@0 
	