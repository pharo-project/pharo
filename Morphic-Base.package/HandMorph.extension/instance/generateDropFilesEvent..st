*Morphic-Base
generateDropFilesEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	"Note: This is still in an experimental phase and will need more work"

	| position buttons modifiers stamp numFiles dragType |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue].
	dragType := evtBuf third.
	position := evtBuf fourth @ evtBuf fifth.
	buttons := 0.
	modifiers := evtBuf sixth.
	buttons := buttons bitOr: (modifiers bitShift: 3).
	numFiles := evtBuf seventh.
	dragType = 4 
		ifTrue: 
			["e.g., drop"

			owner borderWidth: 0.
			^DropFilesEvent new 
				setPosition: position
				contents: numFiles
				hand: self].
	"the others are currently not handled by morphs themselves"
	dragType = 1 
		ifTrue: 
			["experimental drag enter"

			owner
				borderWidth: 4;
				borderColor: owner color asColor negated].
	dragType = 2 
		ifTrue: 
			["experimental drag move"

			].
	dragType = 3 
		ifTrue: 
			["experimental drag leave"

			owner borderWidth: 0].
	^nil