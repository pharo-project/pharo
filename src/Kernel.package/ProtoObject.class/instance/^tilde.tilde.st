comparing
~~ anObject
	"Answer whether the receiver and the argument are different objects
	(do not have the same object pointer)."

	"This seemingly redundant implementation is for performance (and possibly other) reasons. See http://forum.world.st/About-and-td3898409.html for more details"
	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]