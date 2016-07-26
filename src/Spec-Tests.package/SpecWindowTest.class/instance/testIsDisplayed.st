as yet unclassified
testIsDisplayed

	"Test for case: 16800 -> ask a SpecWindow for #isDisplayed always returns true"
	
	| windowModel |

	windowModel := TextModel new openWithSpec.	
	self assert: (windowModel isDisplayed).
	
	windowModel window minimize.	
	self assert: (windowModel isDisplayed).
		
	windowModel window close.
	self assert: (windowModel isDisplayed not).

	