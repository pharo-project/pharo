example
exampleStore
	"This is a pathological case where the method has no arguments, no temporaries, and the instance variable store is followed by the creation of a closure with no copied values."
	instVar1 := 1.
	instVar1 := 2.
	^ thisContext copy 
