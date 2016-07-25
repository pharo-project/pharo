tests - including for identity collections
testIncludesAllOfForIdentityCollections
	"self debug: #testIncludesAllOfAllThere'"
	| collection copyCollection |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	copyCollection := OrderedCollection new.
	collection do: [ :each | copyCollection add: each copy ].
	self assert: (collection includesAll: collection).
	self deny: (collection includesAll: copyCollection).
	self deny: (collection includesAll: {  (copyCollection anyOne)  })