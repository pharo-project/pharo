examples - blocks
exampleBlockNumCopied
	| t1 |
	t1 := OrderedCollection new.
	iVar := {(1 -> 2.3 -> 4.5 -> 6)}.
	^ iVar keysAndValuesDo: [ :key :value | t1 add: (value -> key) ]