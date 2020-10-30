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

{ #category : #api }
SpTSearchable >> disableSearch [
	"Disable searching"

	searchEnabled := false
]

{ #category : #api }
SpTSearchable >> enableSearch [
	"Enable searching"

	searchEnabled := true
]

{ #category : #testing }
SpTSearchable >> hasCustomSearch [
	"Answer true if the list has a custom search defined (See `SpListPresenter>>#searchMatching:`"

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
	"Answer true if search is enabled (See `SpTSearchable>>#enableSearch`)"

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

{ #category : #api }
SpTSearchable >> searchMatching: aBlock [
	"Enables search and defines a block to perform a search on the model objects. 
	 The block receives two parameters: 
		- item (the model element)
		- pattern (the string to match)"

	searchBlock := aBlock.
	self enableSearch
]

{ #category : #private }
SpTSearchable >> searchValueOf: item [ 

	^ self displayValueFor: item
]

{ #category : #'api-events' }
SpTSearchable >> whenSearchEnabledChangedDo: aBlock [
	"Inform when search enabled/disabled has changed.
 	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #searchEnabled whenChangedDo: aBlock
]
