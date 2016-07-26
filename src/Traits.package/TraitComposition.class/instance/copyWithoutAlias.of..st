copying
copyWithoutAlias: aSymbol of: aTrait
	| composition transformation |
	composition := self copyTraitExpression.
	transformation := (composition transformationOfTrait: aTrait).
	^composition
		remove: transformation;
		add: (transformation removeAlias: aSymbol);
		normalizeTransformations;
		yourself