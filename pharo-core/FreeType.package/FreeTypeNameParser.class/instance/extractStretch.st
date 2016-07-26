parsing
extractStretch
	"match and remove last stretch tokens"
	| matches start end |

	extractedStretchValue := LogicalFont stretchRegular.
	(self stretchNames 
		detect: [:each |
			matches := self lastMatchValueSequence: each allButFirst.
			matches ifNotNil:[extractedStretchValue := each first].
			matches notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedStretch := combinedName copyFrom: start to: end.
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.
			"re-tokenize"
			"tokens := self tokenize: combinedName delimiters: delimiters"].	