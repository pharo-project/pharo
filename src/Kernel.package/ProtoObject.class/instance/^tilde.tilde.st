comparing
~~ anObject
	"Primitive. Answer whether the receiver and the argument are different objects
	(do not have the same object pointer). Do not redefine the message ~~ in 
	any other class! Optional (Assuming == is essential). No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."
	
	<primitive: 169>
	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]