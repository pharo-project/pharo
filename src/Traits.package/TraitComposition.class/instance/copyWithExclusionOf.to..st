copying
copyWithExclusionOf: aSymbol to: aTrait
	| composition transformation |
	composition := self copyTraitExpression.
	transformation := (composition transformationOfTrait: aTrait).
	^composition
		remove: transformation;
		add: (transformation addExclusionOf: aSymbol);
		yourself