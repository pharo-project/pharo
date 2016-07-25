parsing
extractWeight
	"match and remove last weight tokens"
	| matches start end |

	extractedWeightValue := LogicalFont weightRegular.
	(self weightNames 
		detect: [:each |
			matches := self lastMatchValueSequence: each allButFirst.
			matches ifNotNil:[extractedWeightValue := each first].
			matches notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedWeight := combinedName copyFrom: start to: end.
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.].	
	(extractedWeight isNil and:[boldFlag])
		ifTrue:["no weight specified in familyName or styleName; force it to be 'Bold'"
			extractedWeight := 'Bold'.
			extractedWeightValue := LogicalFont weightBold]	