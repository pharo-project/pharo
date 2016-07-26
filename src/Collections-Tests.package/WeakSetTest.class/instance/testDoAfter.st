tests
testDoAfter
	"self debug: #testDoAfter"
	| input weakSet array |
	input := (1 to: 11) collect: [ :each | each asString asSymbol ]. "Some symbols might be garbage collected without this variable"
	weakSet := WeakSet withAll: input.
	array := weakSet asArray. "Assume that the elements will have the same order as the internal array of the weakset"
	0 to: array size do: [ :index |
		| element result |
		element := array at: index ifAbsent: nil.
		result := Array new: weakSet size - index streamContents: [ :stream |
			weakSet
				do: [ :each | stream nextPut: each ]
				after: element ].
		self assert: result sort = (array allButFirst: index) sort ]