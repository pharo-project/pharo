Extension { #name : #SpInteractionCancelled }

{ #category : #'*Spec2-Commander2' }
SpInteractionCancelled >> actForSpec [
	"Does nothing on purpose."
	self flag: #TODO. "In the best world, we should delegate the behaviour to the Spec presenter which originated the error."
]
