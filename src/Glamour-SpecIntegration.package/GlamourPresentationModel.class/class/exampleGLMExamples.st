example
exampleGLMExamples
	<gtExample>
	"open the Glamour Examples Browser"
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GLMExamplesBrowser startOn: GLMBasicExamples.
	^ ui openWithSpec