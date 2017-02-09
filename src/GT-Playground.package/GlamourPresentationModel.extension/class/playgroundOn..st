*GT-Playground
playgroundOn: aStringOrText
	"Open a playground with aStringOrText. Execute the following code:"
	"self playgroundOn: '21 + 21'"
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GTPlayground startOn: (GTPlayPage new saveContent: aStringOrText).
	^ ui openWithSpec