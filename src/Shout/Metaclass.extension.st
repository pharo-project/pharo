Extension { #name : 'Metaclass' }

{ #category : '*Shout' }
Metaclass >> hasBindingThatBeginsWith: aString [
	"class and pool vars are accessible from the class side the same as the instance side"
	^self instanceSide hasBindingThatBeginsWith: aString
]
