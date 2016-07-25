rendering
actOnContextChanged: ann 
	ann property = #selectionInterval
		ifTrue: [ 
			ann value notNil 
				ifTrue: [textMorph setSelection: ann value ]
				ifFalse: [textMorph setSelection: (1 to: 0)] ].
"	self flag: 'somehow this disturbs copy paste and cmd+shift+left selection'.
	ann property = #text
		ifTrue: [ 
			textMorph announcer suspendAll: {GLMTextChanged} while: [ 
					textModel changedText: ann value ] ]"