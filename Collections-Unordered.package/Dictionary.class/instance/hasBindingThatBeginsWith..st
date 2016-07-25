testing
hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	
	self keysDo:[:each | 
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false