enumeration
specListDetect: aBlock ifNone: exceptionBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Answer the first element for which aBlock evaluates to true. If none  
	evaluate to true, then evaluate the argument, exceptionBlock."

	self
		specListDo: [ :each | 
			(aBlock value: each)
				ifTrue: [ ^ each ] ].
	^ exceptionBlock value