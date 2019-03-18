Extension { #name : #GlamourPresentationPresenter }

{ #category : #'*GT-Inspector' }
GlamourPresentationPresenter class >> exampleSimpleMethodsBrowser [
	<sampleInstance>
	"Open a Methods Browser on Object."
	|ui|
	ui := self new.
	ui presentationClass: GTSimpleMethodsBrowser startOn: Object.
	^ ui openWithSpec
]
