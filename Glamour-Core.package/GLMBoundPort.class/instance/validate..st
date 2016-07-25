validation
validate: anObject 
	self pane presentations isEmpty ifTrue: [^true].
	^self pane presentations allSatisfy: [ :each | each validate: anObject on: self]