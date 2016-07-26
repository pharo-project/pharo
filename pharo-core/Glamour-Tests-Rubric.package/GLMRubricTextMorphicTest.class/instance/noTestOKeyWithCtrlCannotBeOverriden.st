tests
noTestOKeyWithCtrlCannotBeOverriden
	| composite textMorph overriden shortcut|
	overriden := false.
	composite := GLMCompositePresentation new with: [ :a | a text act: [ :text | overriden := true ] on: $o entitled: 'Override'].
	window := composite openOn: '4'.
	textMorph := self find: RubScrolledTextMorph in: window.
	"self simulateOnlyOneClickOn: textMorph."
	
	shortcut := $o meta asKeyCombination.
	textMorph textArea handleKeystroke: (KeyboardEvent new
		setType: #keystroke
		buttons: shortcut modifier eventCode
		position: textMorph textArea center
		keyValue: shortcut key asciiValue
		charCode: shortcut key asciiValue
		hand: nil
		stamp: DateAndTime now).
	
	self assert: overriden not