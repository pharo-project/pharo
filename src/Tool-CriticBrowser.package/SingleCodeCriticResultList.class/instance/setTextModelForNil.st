initialization
setTextModelForNil

	| text |
	text := rule rationale isEmpty 
		ifTrue: [ rule name ]
		ifFalse: [ rule rationale ]. 
	sourceCodeModel aboutToStyle: false.
	sourceCodeModel text: text.
	sourceCodeModel behavior: nil.
	toolbarModel critic: nil 
	
	
					