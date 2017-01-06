*GT-Inspector
exampleSimpleMethodsBrowser
	<gtExample>
	"Open a Methods Browser on Object."
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GTSimpleMethodsBrowser startOn: Object.
	^ ui openWithSpec