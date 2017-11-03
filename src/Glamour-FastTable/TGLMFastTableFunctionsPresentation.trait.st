"
I am a Trait that manage the differents functionnalities for a FastTable related presentation.

Description
--------------------

My users have to define a #parameters method that need to be a getter for a Dictionary. Then I will store the informations I manage into this dictionary.

I work with all the presentations that use a FastTable as GLMFastLastPresentation, GLMFastTablePresentation or GLMFastTreePresentation.

Public API and Key Messages
--------------------

- #enableFilter 		add a filter to the GLMFastTable. (not compatible with the search)

- #enableSearch 		add a search to the GLMFastTable (A filter will remove the elements that doesn't match when a search will just highlight the matching results).
		
- #enableFilterWithAction: aBlock / #enableFilterWithAction:  aBlock named: aString 		add a filter and an action button. The action is define by a block that can take 3 parameters (the dataSource with the currents elements, the current filter pattern and the selected row of the table)
		
#searchOn: and #filterOn: are not implemented yet.

Example
--------------------

	aGLMFastTablePresentation enableFilterWithAction: [ :dataSource :pattern :item | item browse ] named: 'Browse'. 

Internal Representation and Key Implementation Points.
--------------------

To remember the options I keep some values inside a Dictionary. This dictionary is manage by a presentation and I get it via the #parameters method.
"
Trait {
	#name : #TGLMFastTableFunctionsPresentation,
	#category : #Glamour-FastTable-Presentations
}

{ #category : #accessing }
TGLMFastTableFunctionsPresentation classSide >> defaultRowHeight [
	^ StandardFonts defaultFont pixelSize + 7
]

{ #category : #accessing }
TGLMFastTableFunctionsPresentation >> actionBlock [
	^ self parameters at: 'actionBlock'
]

{ #category : #accessing }
TGLMFastTableFunctionsPresentation >> actionName [
	^ self parameters at: 'actionName' ifAbsent: 'Validate'
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableElementIndex [
	self parameters at: 'withRowIndex' put: true
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableFilter [
	self parameters at: 'isFilterable' put: true
	
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableFilter: aBlock [
	"aBlock receives two parameters: 
		- an item
		- a pattern.
	It should return true if the item should be displayed; false otherwise."
	self enableFilter.
	self parameters at: 'filterBlock' put: aBlock
	
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableFilterWithAction: aBlock [
	self enableFilter.
	self parameters at: 'actionBlock' put: aBlock
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableFilterWithAction: aBlock named: aString [
	self enableFilter.
	self parameters
		at: 'actionBlock' put: aBlock;
		at: 'actionName' put: aString
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> enableSearch [
	self parameters at: 'isSearchable' put: true
]

{ #category : #accessing }
TGLMFastTableFunctionsPresentation >> filterBlock [
	^ self parameters at: 'filterBlock'
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> filterOn: aBlock [
	self error: 'This presentation does not allow filtering with a block for now. In the futur we should have a new filter with FastTable to do that. If you want to give it a try look FTFunction and his subclasses.'
]

{ #category : #testing }
TGLMFastTableFunctionsPresentation >> hasFilterBlock [
	^ self parameters at: 'filterBlock' ifPresent: [ true ] ifAbsent: [ false ]
]

{ #category : #testing }
TGLMFastTableFunctionsPresentation >> isFilterable [
	^ self parameters at: 'isFilterable' ifAbsent: [ false ]
]

{ #category : #testing }
TGLMFastTableFunctionsPresentation >> isSearchable [
	^ self parameters at: 'isSearchable' ifAbsent: [ false ]
]

{ #category : #accessing }
TGLMFastTableFunctionsPresentation >> isWithElementIndex [
	^ self parameters at: 'withRowIndex' ifAbsent: [ false ]
]

{ #category : #testing }
TGLMFastTableFunctionsPresentation >> needActionButton [
	^ self parameters at: 'actionBlock' ifPresent: [ true ] ifAbsent: [ false ]
]

{ #category : #accessing }
TGLMFastTableFunctionsPresentation >> parameters [
	self explicitRequirement
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> rowHeight [
	^ self rowHeightBlock glamourValue: self asGlamorousMultiValue, self entity asGlamorousMultiValue
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> rowHeight: aNumber [
	self parameters at: 'rowHeight' put: aNumber
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> rowHeightBlock [
	^ self parameters 
		at: 'rowHeight'
		ifAbsent: [ self class defaultRowHeight ]
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> searchOn: aBlock [
	self error: 'This presentation does not allow searching with a block for now. In the futur we should have a new filter with FastTable to do that. If you want to give it a try look FTFunction and his subclasses.'
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> shouldUseCache [
	^ self parameters 
		at: 'shouldUseCache'
		ifAbsent: [ false ]
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> shouldUseCache: aBoolean [
	^ self parameters 
		at: 'shouldUseCache'
		put: aBoolean
]

{ #category : #'public api' }
TGLMFastTableFunctionsPresentation >> useCache [
	self parameters at: 'shouldUseCache' put: true
]
