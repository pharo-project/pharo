examples-blocks-optimized
exampleToDoInsideBlock
	| tmp1 block |
	tmp1 := {1.2.
	3.4} asOrderedCollection.
	block := [ (tmp1 at: 1) to: (tmp1 at: 4) do: [ :arg1 | Transcript show: arg1 ] ]