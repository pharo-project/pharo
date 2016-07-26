parsing
extractUpright
	"extract from current combined name.
	answer new combinedName"
	| normalTok start end | 

	normalTok := tokens reversed 
		detect: [:tok | 
			(self normalNames 
				detect: [:str | str asLowercase = tok first asLowercase] 
				ifNone:[]) notNil ]
		ifNone:[].
	normalTok ifNotNil:[
		"remove it from combinedName"
		start := normalTok second.
		end :=  normalTok third.
		extractedUpright := combinedName copyFrom: start to: end.
		[start > 1 and:[delimiters includes: (combinedName at: start - 1)]]
			whileTrue:[start := start - 1].
		[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]]
			whileTrue:[end := end + 1].		
		combinedName := combinedName copyReplaceFrom: start to: end with: ' '].

	