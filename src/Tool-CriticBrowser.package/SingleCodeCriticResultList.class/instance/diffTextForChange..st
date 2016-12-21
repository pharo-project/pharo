private
diffTextForChange: aRefactoryChange
	| text builder |

	text := Text new.
	builder := PrettyTextDiffBuilder new.
	aRefactoryChange changes
		do: [ :chng |
			builder
				from: chng oldVersionTextToDisplay
				to: chng textToDisplay.
			text append: builder buildDisplayPatch ]
		separatedBy: [ text append: self diffSeparator ].
	
	^ text