parsing
extractSlant

	|  matches start end |

	"match and remove last italic/oblique token"
	extractedSlant := nil.
	extractedSlantValue := LogicalFont slantRegular. "not italic or oblique"
	(self italicAndObliqueNames 
		detect: [:each |
			(matches := self lastMatchValueSequence: {each}) notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedSlant := combinedName copyFrom: start to: end.
			"extractedSlantValue := (self italicNames includes: extractedSlant asLowercase)
				ifTrue:[1]
				ifFalse:[2]."
			extractedSlantValue := LogicalFont slantItalic. "treat italic and oblique the same, as italic"
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.].
	(extractedSlant isNil and:[italicFlag])
		ifTrue:["no italic specified in familyName or styleName; force it to be 'Italic'"
			extractedSlant := 'Italic'.
			extractedSlantValue := LogicalFont slantItalic]		