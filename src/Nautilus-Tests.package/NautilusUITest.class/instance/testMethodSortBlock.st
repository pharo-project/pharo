tests
testMethodSortBlock
	"Test that the default sort block sorts binary methods ahead of
	non-binary methods and sorts using Character values otherwise."

	| sortBlock |
	sortBlock := nautilusUI methodSortBlock.

	"These classes define at leat one binary selector, #~= or #|, which
	comes after any non-binary selector when sorted by Character value."
	{Object.
	Collection}
		do: [ :class | 
			| methods binaryMethods nonBinaryMethods sortedMethods |
			methods := class methods.
			binaryMethods := methods select: [ :each | each selector isBinary ].
			nonBinaryMethods := methods reject: [ :each | each selector isBinary ].
			sortedMethods :=
				(binaryMethods sorted: [ :a :b | a selector <= b selector ]),
					(nonBinaryMethods sorted: [ :a :b | a selector <= b selector ]).
			self
				assert: (methods sorted: sortBlock) = sortedMethods;
				assert: (methods reversed sorted: sortBlock) = sortedMethods;
				assert: (sortedMethods sorted: sortBlock) = sortedMethods;
				assert: (sortedMethods reversed sorted: sortBlock) = sortedMethods ]