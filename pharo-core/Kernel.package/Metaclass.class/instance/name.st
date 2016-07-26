accessing
name
	"Answer a String that is the name of the receiver, either 'Metaclass' or 
	the name of the receiver's class followed by ' class'."

	^ thisClass isNil
		ifTrue: [ 'a Metaclass' ]
		ifFalse: [ thisClass name asString, ' class' ]