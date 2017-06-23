accessing method dictionary
ultimateSourceCodeAt: selector ifAbsent: aBlock
	"Return the source code at selector, deferring to superclass if necessary"
	^ self sourceCodeAt: selector ifAbsent:
		[self superclass
			ifNil:
				[aBlock value]
			 ifNotNil:
				[self superclass ultimateSourceCodeAt: selector ifAbsent: aBlock]]