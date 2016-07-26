tests
testSimulateClick
	"self run: #testSimulateClick"

	morph := TextMorph new contents: ''; openInWorld.	
	morph simulateClick.
	self assert: morph hasKeyboardFocus.