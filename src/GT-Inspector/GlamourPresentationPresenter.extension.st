Extension { #name : #GlamourPresentationPresenter }

{ #category : #'*GT-Inspector' }
GlamourPresentationPresenter class >> exampleInspectorOn42 [
	<sampleInstance>
	"open an inspector on 42"
	^ self inspectorOn: 42
]

{ #category : #'*GT-Inspector' }
GlamourPresentationPresenter class >> exampleSimpleMethodsBrowser [
	<sampleInstance>
	"Open a Methods Browser on Object."
	|ui|
	ui := self new.
	ui presentationClass: GTSimpleMethodsBrowser startOn: Object.
	^ ui openWithSpec
]

{ #category : #'*GT-Inspector' }
GlamourPresentationPresenter class >> inspectorOn: anObject [
	"Open an inspector on anObject. 
	Execute the following example:
	self inspectorOn: 42.
	self inspectorOn: self new."
	|ui|
	ui := GlamourPresentationPresenter new.
	ui presentationClass: GTInspector startOn: anObject.
	^ ui openWithSpec
]
