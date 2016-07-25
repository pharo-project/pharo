examples
highlightIncompleteCode
	| styler text code |
	
	
	code := 'thisIsIncompleteMethod: argument
		| dork zork |
		Wo
	
	'.
	
	styler := self new.
	text := styler styleCode: code inContext: self "class" isMethod: true.

	^ text editInWindow.