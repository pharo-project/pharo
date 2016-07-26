testing
testTraitsMethodClassSanity
	"self debug: #testTraitsMethodClassSanity"
	
	(Smalltalk globals allTraits flatCollect: #traitUsers) asSet do: [ :each |
		each selectorsDo: [ :selector |
			self should: [ (each >> selector) methodClass == each ] ] ]