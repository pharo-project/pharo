comparing
= anObject 
	"Answer whether the receiver and the argument represent the same 
	object. If = is redefined in any subclass, consider also redefining the 
	message hash."

	^self == anObject 