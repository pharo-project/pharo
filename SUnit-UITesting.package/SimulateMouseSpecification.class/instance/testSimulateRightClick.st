tests
testSimulateRightClick
	"self run: #testSimulateRightClick"
	
	| menu |
	morph := Smalltalk tools workspace open.
	menu := self menuOpenedDuring: [ morph simulateRightClick ].
	self assert: menu isNil not.