initialize-release
predicate: aBlock
	"This link will match any single character for which <aBlock>
	evaluates to true."

	aBlock numArgs ~= 1 ifTrue: [self error: 'bad predicate block'].
	predicate := aBlock.
	^self