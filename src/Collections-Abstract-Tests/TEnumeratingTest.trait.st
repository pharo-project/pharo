"
Test of basic functions in Collections that are in the protocol enumerating

"
Trait {
	#name : 'TEnumeratingTest',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'requirements' }
TEnumeratingTest >> collectionOfCollectionsOfInts [
	^self explicitRequirement
]

{ #category : 'requirements' }
TEnumeratingTest >> collectionOfCollectionsOfStrings [
	^self explicitRequirement
]

{ #category : 'requirements' }
TEnumeratingTest >> collectionWithCharacters [
	^self explicitRequirement
]

{ #category : 'requirements' }
TEnumeratingTest >> simpleCollection [
	^self explicitRequirement
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testCartesianProductDo [

   | collection index elements |

   collection := #(1 2).
	index := 1.
	elements := #(#(1 1) #(1 2) #(2 1) #(2 2)).
   collection permutationsWithRepetitionsDo: [ : product |
	   self 
			assertCollection: product hasSameElements: (elements at: index).
			index := index + 1 ]
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testCartesianProductDo3Elements [

   | collection index elements |
   collection := #(1 2 3).
	index := 1.
	elements := #(#(1 1 1) #(1 1 2) #(1 1 3) #(1 2 1) #(1 2 2) #(1 2 3) #(1 3 1) #(1 3 2) #(1 3 3) #(2 1 1) #(2 1 2) #(2 1 3) #(2 2 1) #(2 2 2) #(2 2 3) #(2 3 1) #(2 3 2) #(2 3 3) #(3 1 1) #(3 1 2) #(3 1 3) #(3 2 1) #(3 2 2) #(3 2 3) #(3 3 1) #(3 3 2) #(3 3 3)).
   collection permutationsWithRepetitionsDo: [ : product |
	   self 
			assert: product
			equals: (elements at: index).
		index := index + 1 ]
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testCartesianProductOfSize [

   | collection result |
   collection := #(1 2 3).
   result := collection permutationsWithRepetitionsOfSize: 2.
   self assert: result size equals: 9.
   self assert: (result includesAll: #(#(1 1) #(1 2) #(1 3) #(2 1) #(2 2) #(2 3) #(3 1) #(3 2) #(3 3))).
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testFlatCollect [

	self assert: (self simpleCollection flatCollect: [ :x | { x } ]) equals: self simpleCollection.
	self assert: (self simpleCollection flatCollect: [ :x | { x } ]) species = self simpleCollection species.

	self
		assert: (self collectionOfCollectionsOfInts flatCollect: [ :x | { x } ])
		equals: self collectionOfCollectionsOfInts.
	self
		assert: (self collectionWithCharacters flatCollect: [ :x | { x } ])
		equals: self collectionWithCharacters.
	self
		assert: (self collectionOfCollectionsOfStrings flatCollect: [ :x | { x } ])
		equals: self collectionOfCollectionsOfStrings
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testFlatCollectAs [

	self
		assert: (self simpleCollection flatCollect: [ :x | { x }, { x } ] as: IdentitySet)
		equals: self simpleCollection asIdentitySet.
	self
		assert: (#(foo bar baz) flatCollect: #yourself as: String)
		equals: 'foobarbaz'.
	self
		assert: (#(#(1 2) #(4 3)) flatCollect: #yourself as: Array)
		equals: #(1 2 4 3)
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testFlatCollectAsSet [

	self
		assert: (self simpleCollection flatCollectAsSet: [ :x | { x }, { x } ])
		equals: self simpleCollection asSet
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testFlatCollectAsWithEmptyCollection [

	self
		assert: (self empty flatCollect: [ :x | { x }, { x } ] as: IdentitySet)
		equals: self empty asIdentitySet
]

{ #category : 'tests - enumerating' }
TEnumeratingTest >> testFlattened [

	self assert: self simpleCollection flattened equals: #(1 8 3).
	self assert: self collectionOfCollectionsOfInts flattened equals: #( 1 2 3 4 5 6 ).
	self assert: self collectionWithCharacters flattened equals: #($a $x $d $c $m).
	self assert: self collectionOfCollectionsOfStrings flattened equals: #('foo' 'bar' 'zorg').
	"we do not try to flatten CompiledMethods"
	self assert: { Object>>#halt } flattened equals:  { Object>>#halt }
]
