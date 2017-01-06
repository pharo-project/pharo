*GT-Inspector
inspectorOn: anObject
	"Open an inspector on 42. Execute the following code:"
	"self inspectorOn: 42."
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GTInspector startOn: 42.
	^ ui openWithSpec