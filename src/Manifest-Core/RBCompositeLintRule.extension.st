Extension { #name : #RBCompositeLintRule }

{ #category : #'*Manifest-Core' }
RBCompositeLintRule class >> allGoodRules [
	^ self rules: (self rulesClassifiedFor: RBLintRule) name: 'All Rules'
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
