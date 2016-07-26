initialization
setTextModelForNil

	| text |
	text := rule longDescription isEmpty 
		ifTrue: [ rule name ]
		ifFalse: [ rule longDescription ]. 
	sourceCodeModel aboutToStyle: false.
	sourceCodeModel text: text.
	sourceCodeModel behavior: nil.
	toolbarModel critic: nil 
	
	
					