morph creation
newTaskbarButtonIn: aTaskbar for: aWindow
	"Answer a taskbar button morph for the given window."

	|lm lab button labSize|
	labSize := (150 // (aTaskbar tasks size + 1) max: 10) min: 30.
	lab := (self buttonLabelForText: (aWindow taskbarLabel truncateWithElipsisTo: labSize)).
	aWindow isActive
		ifTrue: [ lab emphasis: 1 "1 -> bold" ].
	lm := self
		newRowIn: aTaskbar
		for: {(aWindow taskbarIcon ifNil: [^nil]) asMorph. lab}.
	lm cellInset: 2.
	button := self
		newButtonIn: aTaskbar
		for: aWindow
		getState: #isActive
		action: #taskbarButtonClicked
		arguments: #()
		getEnabled: nil
		label: lm
		help: nil.
	button
		useSquareCorners;
		wantsYellowButtonMenu: true;
		getMenuSelector: #taskbarButtonMenu:;
		on: #mouseEnter send: #taskbarButtonEntered:event:in: to: aWindow withValue: button;
		on: #mouseLeave send: #taskbarButtonLeft:event:in: to: aWindow withValue: button.
	aWindow isCollapsed
		ifTrue:  [ lab color: (self taskbarButtonLabelColorForCollapsed: button) ]
		ifFalse: [ lab color: (self taskbarButtonLabelColorForExpanded: button). ].
	^button