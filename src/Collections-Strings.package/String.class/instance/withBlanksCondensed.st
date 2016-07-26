converting
withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks (separators) removed
	 and consecutive white spaces (separators) condensed to the first one."

	| trimmed lastBlank |
	trimmed := self trimBoth.
	^ String streamContents: [ :stream | 
			lastBlank := false.
			trimmed
				do: [ :eachChar | 
					(eachChar isSeparator and: [ lastBlank ])
						ifFalse: [ stream nextPut: eachChar ].
					lastBlank := eachChar isSeparator ] ]

	" ' abc  d   ' withBlanksCondensed"