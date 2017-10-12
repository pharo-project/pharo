tests
testIsDisplayed

	"Test for case: 16800 -> ask a SpecWindow for #isDisplayed always returns true"
	
	| windowModel |

	windowModel := TextPresenter new openWithSpec.	
	self assert: (windowModel isDisplayed).
	
	windowModel window minimize.	
	self assert: (windowModel isDisplayed).
		
	windowModel window close.
	self assert: (windowModel isDisplayed not).

	