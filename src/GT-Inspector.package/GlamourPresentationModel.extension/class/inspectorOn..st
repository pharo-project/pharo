*GT-Inspector
inspectorOn: anObject
	"Open an inspector on anObject. 
	Execute the following example:
	self inspectorOn: 42.
	self inspectorOn: self new."
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GTInspector startOn: anObject.
	^ ui openWithSpec