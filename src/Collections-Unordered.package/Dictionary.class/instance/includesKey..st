testing
includesKey: key 
	"Answer whether the receiver has a key equal to the argument, key."
	
	^ (array at: (self scanFor: key)) ~~ nil 
	"We could use #notNil here, but ProtoObject doesn't understand it."