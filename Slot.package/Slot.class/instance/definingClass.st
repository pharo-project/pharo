queries
definingClass
	^(Smalltalk globals allClasses flatCollect: [:each | {each . each classSide} ]) 
		detect: [ :class | class classLayout definesSlot: self] 
		ifNone: [ nil ]