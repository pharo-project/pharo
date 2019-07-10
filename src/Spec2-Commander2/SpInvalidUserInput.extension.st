Extension { #name : #SpInvalidUserInput }

{ #category : #'*Spec2-Commander2' }
SpInvalidUserInput >> actForSpec [
	"Does nothing on purpose."
	UIManager default
		inform: self messageText.
	self flag: #TODO "In the best world, we should delegate the behaviour to the Spec presenter which originated the error."
]
