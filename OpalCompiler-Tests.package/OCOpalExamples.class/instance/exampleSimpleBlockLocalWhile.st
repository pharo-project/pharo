examples-blocks-optimized
exampleSimpleBlockLocalWhile
	|a|
	a := true.
	^[: b | [a] whileTrue: [ | hallo |  a := false. hallo := 1 . hallo]]value: 1.