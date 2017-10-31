api
emphasis: anEmphasis
	"accepts a symbol or an array of symbols: #bold, #italic, #underline, #narrow, #struck"

	emphasis := anEmphasis isSymbol
		ifTrue: [ {anEmphasis} ]
		ifFalse: [ anEmphasis ].
	self changed: #emphasis: with: {emphasis}