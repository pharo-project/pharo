Extension { #name : #CommandLineUIManager }

{ #category : #'*Jobs' }
CommandLineUIManager >> informUserDuring: aBlock [
	self
		displayProgress: ''
		from: 1 to: 100
		during: aBlock
]
