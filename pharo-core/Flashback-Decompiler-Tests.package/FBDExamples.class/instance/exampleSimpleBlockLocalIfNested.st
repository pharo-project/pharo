examples-blocks
exampleSimpleBlockLocalIfNested
	^true ifTrue: [| hallo |  [  hallo := 1 . hallo] value] .