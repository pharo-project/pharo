accessing
rowMorphForColumn: aTreeColumn
	^ self item ifNotNil: [:i | self asString asMorph] 