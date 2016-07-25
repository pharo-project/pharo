accessing
emphasis
	| style |
	^ emphasis
		ifNil: [style := SHPreferences customStyleTable
						detect: [:e | self tokens first = e first].
			style size > 2
				ifTrue: [emphasis := style third.
					emphasis isSymbol
						ifFalse: [emphasis := ' ' join: emphasis].
					emphasis]
				ifFalse: [#normal]]