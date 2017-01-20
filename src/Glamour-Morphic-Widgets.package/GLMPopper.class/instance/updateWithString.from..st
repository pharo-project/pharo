private
updateWithString: string from: aMorph
	textMorph textArea
		updateTextWith: string;
		takeKeyboardFocus;
		on: Character escape do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph takeKeyboardFocus ];
		on: PharoShortcuts current cancelShortcut do: [ 
			self delete.
			aMorph textArea editor cancel.
			aMorph takeKeyboardFocus ];
		on: Character backspace do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph takeKeyboardFocus ];
		onAnnouncement: MorphLostFocus do: [ :ann | 
			self delete.
			aMorph textArea removeHighlightSegment.
			"If this event is triggered then another morph already has the focus.
			Do not attempt to give the focus back to the original morph" ]