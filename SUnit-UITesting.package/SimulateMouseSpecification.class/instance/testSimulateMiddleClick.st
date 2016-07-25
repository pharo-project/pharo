tests
testSimulateMiddleClick
	"self run: #testSimulateMiddleClick"
	
	morph := MorphHandlingMiddleButton new openInWorld.
	morph simulateMiddleClick.
	self assert: morph wasClickedWithMiddleButton.
