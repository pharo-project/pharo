Extension { #name : #RBCompositeLintRule }

{ #category : #'*Manifest-Core' }
RBCompositeLintRule class >> allGoodRules [
	| rules |
	rules := self rules: (self rulesClassifiedFor: RBLintRule) name: 'All Rules'.
	self removedRules do: [ :r | rules removeRule: r ].
	rules removeRule: (RBCompositeLintRule new name: 'Spelling').
	rules removeRule: RBMissingTranslationsInMenusRule new.
	^ rules
]

{ #category : #'*Manifest-Core' }
RBCompositeLintRule class >> removedRules [
	^ {(RBCompositeLintRule new name: 'Spelling').  
		RBMissingTranslationsInMenusRule new.
		RBAssignmentWithoutEffectRule new.
		RBUtilityMethodsRule new. }
	

]

{ #category : #'*Manifest-Core' }
RBCompositeLintRule class >> rulesClassifiedFor: aRuleClass [
	| groups rules |
	groups := Dictionary new.
	(self rulesFor: aRuleClass) do: [ :each |
		(groups at: each group ifAbsentPut: [ OrderedCollection new ])
			addLast: each ].
	rules := SortedCollection sortBlock: [ :a :b | a name <= b name ].
	groups keysAndValuesDo: [ :group :elements |
		rules addLast: (RBCompositeLintRule 
			rules: elements asArray
			name: group) ].
	^ rules asArray
]
