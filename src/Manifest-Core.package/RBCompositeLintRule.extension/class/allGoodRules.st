*Manifest-Core
allGoodRules
	| rules |
	rules := self rules: (self rulesClassifiedFor: RBLintRule) name: 'All Rules'.
	self removedRules do: [ :r | rules removeRule: r ].
	rules removeRule: (RBCompositeLintRule new name: 'Spelling').
	rules removeRule: RBMissingTranslationsInMenusRule new.
	^ rules