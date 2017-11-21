"
I provide an API for adding table columns having various  properties. I should be used by glamour presentations representing tables. I also provide the hook methods called be the Glamour renderer (see the *callbacks* protocol).

Public API and Key Messages
--------------------
Main methods:

- #column: evaluated:		        to add a configured column; there are many other methods that adds columns and configure different properties
		
- #addColumn: 		                  to directly add a column object
		
- #hideHeader                                  to hide the header of all columns
"
Trait {
	#name : #TGLMScriptingAPITablePresentation,
	#category : #Glamour-Presentations
}

{ #category : #accessing }
TGLMScriptingAPITablePresentation classSide >> defaultIntercellSpacing [
	^ 3@1 
]

{ #category : #accessing }
TGLMScriptingAPITablePresentation >> addColumn: aColumn [
	self columns add: aColumn
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock modified: aModifiedBlock [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString;
		computation: aBlock;
		modifiedBlock: aModifiedBlock)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock sortedBy: aSortingBlock [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		sortBlock: aSortingBlock)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock tags: anObject [ 
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tags: anObject)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock tags: anObject filtered: anotherObject [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tagsBlock: anObject;
		tagsFilter: anotherObject)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock tags: anObject sortedBy: aSortingBlock [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tags: anObject;
		sortBlock: aSortingBlock)
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock width: aNumber [ 
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		yourself)	
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock width: aNumber tags: anObject [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		tags: anObject;
		yourself)	
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> column: aBlockOrString evaluated: aBlock width: aNumber tags: anObject sortedBy: aSortingBlock [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		tags: anObject;
		sortBlock: aSortingBlock
		yourself)	
]

{ #category : #callbacks }
TGLMScriptingAPITablePresentation >> column: aGlamourColumn valueFor: anObject [
	^ aGlamourColumn computation glamourValue: 
		(anObject asGlamorousMultiValue,
		 self entity asGlamorousMultiValue,
		 self asGlamorousMultiValue)
]

{ #category : #accessing }
TGLMScriptingAPITablePresentation >> columns [
	self explicitRequirement
]

{ #category : #scripting }
TGLMScriptingAPITablePresentation >> hideHeader [
	self explicitRequirement
]

{ #category : #accessing }
TGLMScriptingAPITablePresentation >> shouldShowHeader [
	self explicitRequirement
]

{ #category : #callbacks }
TGLMScriptingAPITablePresentation >> titleValueOfColumn: aColumn [
	^ aColumn title glamourValue: self entity
]
