tests
testSettingViewExtent
	"layout should answer text position from visual position"
	
	
	view extent:100@100.
	
	self assert: view extent equals:100@100