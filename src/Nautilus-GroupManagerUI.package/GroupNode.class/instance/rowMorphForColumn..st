accessing
rowMorphForColumn: aTreeColumn

	^ self item ifNotNil: [:i | i name asMorph] 