"
I represent a context of the full browser, i.e., a selected class, package, method, group of methods,...

I extend superclass interface with class and method groups information. 
And I keep meta level scope of the browser.
 
Internal Representation and Key Implementation Points.

Instance Variables
	metaLevelScope:		<ClyMetaLevelClassScope>
"
Class {
	#name : #ClyFullBrowserContext,
	#superclass : #ClySystemBrowserContext,
	#instVars : [
		'metaLevelScope'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Contexts'
}

{ #category : #accessing }
ClyFullBrowserContext >> currentMetaLevelOf: aClass [
	^metaLevelScope metaLevelOf: aClass
]

{ #category : #'selection-packages' }
ClyFullBrowserContext >> isClassTagSelected [
	| groups |
	groups := tool classGroupSelection.
	groups isEmpty ifTrue: [ ^false ].
	^groups lastSelectedItem type isBasedOnClassTag 
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> isMethodGroupSelected [
	^self selectedMethodGroupItems notEmpty
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> isMethodTagSelected [

	self isMethodGroupSelected ifFalse: [ ^false ].
	
	^self selectedMethodGroupClass isBasedOnMethodTag
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> isProjectChildSelected [
	^self selectedProjectChildItems notEmpty
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> isProjectSelected [
	^self selectedProjectItems notEmpty
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> isSelectedMethodGroupEditable [
	
	self isMethodGroupSelected ifFalse: [ ^false ].
	
	^self selectedMethodGroupClass isEditableGroup 
]

{ #category : #testing }
ClyFullBrowserContext >> isSimilarTo: anotherBrowserContext [
	^(super isSimilarTo: anotherBrowserContext)
		and: [ metaLevelScope == anotherBrowserContext metaLevelScope ]
]

{ #category : #'selection-packages' }
ClyFullBrowserContext >> lastSelectedClassTag [
	| tagItem |
	tagItem := tool classGroupSelection items 
		detect: [ :each | each type isBasedOnClassTag ].
	^tagItem name
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> lastSelectedMethodGroup [
	^self lastSelectedObjectIn: self selectedMethodGroups
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> lastSelectedProject [
	^self lastSelectedObjectIn: self selectedProjects
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> lastSelectedProjectChild [
	^self lastSelectedObjectIn: self selectedProjectChildren
]

{ #category : #accessing }
ClyFullBrowserContext >> metaLevelScope [
	^ metaLevelScope
]

{ #category : #accessing }
ClyFullBrowserContext >> metaLevelScope: anObject [
	metaLevelScope := anObject
]

{ #category : #'selection-classes' }
ClyFullBrowserContext >> selectedClassItems [
	^tool classSelection items
]

{ #category : #'selection-classes' }
ClyFullBrowserContext >> selectedClassSide [
	"The browser (tool) has selected classes but the side from where browser show it can be instance side of class side.
	So this method return actual 'meta side' of selected class"
	^ tool currentMetaLevelOf: self lastSelectedClass
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroupClass [

	^(self lastSelectedObjectIn: self selectedMethodGroupItems) type
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroupItems [
	^tool methodGroupSelection items
]

{ #category : #'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroups [
	^self actualSelectionFrom: self selectedMethodGroupItems 
]

{ #category : #'selection-methods' }
ClyFullBrowserContext >> selectedMethodItems [
	^tool methodSelection items
]

{ #category : #'selection-packages' }
ClyFullBrowserContext >> selectedPackageItems [
	^tool packageSelection items
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> selectedProjectChildItems [
	^tool projectChildrenSelection items
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> selectedProjectChildren [
	^self actualSelectionFrom: self selectedProjectChildItems 
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> selectedProjectItems [
	^tool projectSelection items
]

{ #category : #'selection-projects' }
ClyFullBrowserContext >> selectedProjects [
	^self actualSelectionFrom: self selectedProjectItems
]

{ #category : #'tool controlling' }
ClyFullBrowserContext >> showMethodTag: tagName [
	tool selectMethodGroupNamed: tagName
]

{ #category : #'tool controlling' }
ClyFullBrowserContext >> showPackage: aPackage [
	tool selectPackage: aPackage
]

{ #category : #'tool controlling' }
ClyFullBrowserContext >> showPackage: aPackage atClassTag: tagName [
	tool selectPackage: aPackage atClassTag: tagName
]

{ #category : #'tool controlling' }
ClyFullBrowserContext >> showVariableNamed: varName [
	tool selectVariableNamed: varName
]

{ #category : #accessing }
ClyFullBrowserContext >> tool: aBrowser [
	super tool: aBrowser.
	metaLevelScope := aBrowser metaLevelScope
]
