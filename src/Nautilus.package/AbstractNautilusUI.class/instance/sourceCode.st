source code area
sourceCode

	^self selectedClass 
		ifNil: [ self defaultClassDescriptor]
		ifNotNil: [:class | self selectedMethod 
			ifNil: [ self selectedCategory 
					ifNotNil: [ 
							class sourceCodeTemplate]
					ifNil: [ self selectedClassDescription]]
			ifNotNil: [ 
					self selectedMethodSource]].
		