Extension { #name : #SpButtonPresenter }

{ #category : #'*Spec2-Deprecated' }
SpButtonPresenter >> getAction [
	"get the block performed when the button is clicked"

	self deprecated: 'Use #action instead' transformWith: '`@receiver getAction' -> '`@receiver action'.
	^ self action
]
