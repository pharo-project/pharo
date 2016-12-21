initialization
setTextModelForTransformationRule: selection
	
	sourceCodeModel behavior: nil.
	sourceCodeModel text: (
		self diffTextForChange: selection change).
	sourceCodeModel aboutToStyle: false.

									
					