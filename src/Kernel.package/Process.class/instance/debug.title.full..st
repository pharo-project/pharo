debugging
debug: context title: title full: bool
	
	^ UIManager default
		debugProcess: self 
		context: context 
		label: title 
		fullView: bool