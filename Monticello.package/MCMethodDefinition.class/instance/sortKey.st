comparing
sortKey
	^ self className, '.', (self classIsMeta ifTrue: ['meta'] ifFalse: ['nonmeta']), '.', self selector