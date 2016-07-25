examples
useCase

	| styler text |
	
	styler := self new.
	text := styler styleMethod: (self >> #initializeStylesFor:).
	^ text editInWindow.