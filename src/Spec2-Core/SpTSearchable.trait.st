"
I add search capability to lists/tables and trees. 
I implement basic common API but real implementation (as always) needs to be done in the backend adapters. 
"
Trait {
	#name : #SpTSearchable,
	#instVars : [
		'#searchEnabled => SpObservableSlot',
		'#searchBlock'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #accessing }
SpTSearchable >> disableSearch [

	searchEnabled := false
]

{ #category : #accessing }
SpTSearchable >> enableSearch [

	searchEnabled := true
]

{ #category : #testing }
SpTSearchable >> hasCustomSearch [

	^ searchBlock notNil
]

{ #category : #initialization }
SpTSearchable >> initialize [
	
	self class initializeSlots: self.
	super initialize.
	self initializeTSearchable
]

{ #category : #initialization }
SpTSearchable >> initializeTSearchable [
	
	self searchMatching: [ :item :pattern | 
		self performDefaultSearch: item matching: pattern ].
]

{ #category : #testing }
SpTSearchable >> isSearchEnabled [

	^ searchEnabled
]

{ #category : #private }
SpTSearchable >> performDefaultSearch: item matching: pattern [
	| text |
	
	text := (self searchValueOf: item) trimBoth asLowercase.
	^ text beginsWith: pattern
]

{ #category : #private }
SpTSearchable >> performSearch: item matching: pattern [

	^ searchBlock 
		value: item 
		value: pattern
]

{ #category : #accessing }
SpTSearchable >> searchMatching: aBlock [
	"Defines a block to perform a search on the model objects. 
	 The block receives two parameters: 
		- item (the model element)
		- pattern (the string to match)"

	searchBlock := aBlock.
	self enableSearch
]

{ #category : #private }
SpTSearchable >> searchValueOf: item [ 

	^ self displayValueOf: item
]

{ #category : #'api-events' }
SpTSearchable >> whenSearchChangedDo: aBlock [

	self property: #searchEnabled whenChangedDo: aBlock
]
