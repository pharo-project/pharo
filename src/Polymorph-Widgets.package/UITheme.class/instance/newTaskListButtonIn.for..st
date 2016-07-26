morph creation
newTaskListButtonIn: aTasklist for: aTask
	"Answer a tasklist button morph for the given task."
	
	|lm lab button |
	lab := (self buttonLabelForText: (aTask label contractTo: 40)).
	aTask isActive
		ifTrue: [ lab emphasis: 1 "1 -> bold" ].
	lm := self
		newRowIn: aTasklist
		for: {(aTask icon ifNil: [self smallWindowIcon]) asMorph. lab}.
	button := self
		newButtonIn: aTasklist
		for: aTask morph
		getState: #isActive
		action: #buttonClickedForTaskList:
		arguments: {aTasklist}
		getEnabled: nil
		label: lm
		help: nil.
	button
		useSquareCorners;
		onColor: (self taskbarMinimizedButtonColorFor: button)
		offColor: (aTask isActive
				ifTrue: [self taskbarActiveButtonColorFor: button]
				ifFalse: [self taskbarButtonColorFor: button]);
		hResizing: #spaceFill.
	lab color: (self taskbarButtonLabelColorFor: button).
	button model: aTask.
	^button