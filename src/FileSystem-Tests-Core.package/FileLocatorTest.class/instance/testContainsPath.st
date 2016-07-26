compatibility tests
testContainsPath
	"self debug: #testContainsPath"
	
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle') path).