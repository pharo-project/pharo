tests - including for identity collections
testIncludesAnyOfForIdentityCollections
	"self debug: #testIncludesAnyOfAllThere'"
	| collection copyCollection |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	copyCollection := OrderedCollection new.
	collection do: [ :each | copyCollection add: each copy ].
	self deny: (collection includesAny: copyCollection).
	self assert: (collection includesAny: {  (collection anyOne)  })