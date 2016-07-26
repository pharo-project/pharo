private
actOnContextChanged: ann
	ann property = #selectionInterval
		ifTrue: [
			ann value notNil 
				ifTrue: [textMorph setSelection: ann value ]
				ifFalse: [textMorph setSelection: (1 to: 0)] ]