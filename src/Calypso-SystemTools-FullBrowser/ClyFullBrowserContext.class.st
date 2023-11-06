"
I represent a context of the full browser, i.e., a selected class, package, method, group of methods,...

I extend superclass interface with class and method groups information. 
And I keep meta level scope of the browser.
 
Internal Representation and Key Implementation Points.

Instance Variables
	metaLevelScope:		<ClyMetaLevelClassScope>
"
Class {
	#name : 'ClyFullBrowserContext',
	#superclass : 'ClySystemBrowserContext',
	#instVars : [
		'metaLevelScope'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'accessing' }
ClyFullBrowserContext >> currentMetaLevelOf: aClass [
	^metaLevelScope metaLevelOf: aClass
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> isMethodGroupSelected [
	^self selectedMethodGroupItems notEmpty
]

{ #category : 'selection-packages' }
ClyFullBrowserContext >> isPackageTagSelected [
	| groups |
	groups := tool classGroupSelection.
	groups isEmpty ifTrue: [ ^false ].
	^groups lastSelectedItem type isBasedOnPackageTag
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> isProtocolSelected [

	self isMethodGroupSelected ifFalse: [ ^false ].

	^self selectedMethodGroupClass isBasedOnProtocol
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> isSelectedMethodGroupEditable [

	self isMethodGroupSelected ifFalse: [ ^false ].

	^self selectedMethodGroupClass isEditableGroup
]

{ #category : 'testing' }
ClyFullBrowserContext >> isSimilarTo: anotherBrowserContext [
	^(super isSimilarTo: anotherBrowserContext)
		and: [ metaLevelScope == anotherBrowserContext metaLevelScope ]
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> lastSelectedMethodGroup [
	^self lastSelectedObjectIn: self selectedMethodGroups
]

{ #category : 'selection-packages' }
ClyFullBrowserContext >> lastSelectedPackageTag [

	^ (tool classGroupSelection items detect: [ :each | each type isBasedOnPackageTag ]) name
]

{ #category : 'accessing' }
ClyFullBrowserContext >> metaLevelScope [
	^ metaLevelScope
]

{ #category : 'accessing' }
ClyFullBrowserContext >> metaLevelScope: anObject [
	metaLevelScope := anObject
]

{ #category : 'selection-classes' }
ClyFullBrowserContext >> selectedClassItems [
	^tool classSelection items
]

{ #category : 'selection-classes' }
ClyFullBrowserContext >> selectedClassSide [
	"The browser (tool) has selected classes but the side from where browser show it can be instance side of class side.
	So this method return actual 'meta side' of selected class"
	^ tool currentMetaLevelOf: self lastSelectedClass
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroupClass [

	^(self lastSelectedObjectIn: self selectedMethodGroupItems) type
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroupItems [
	^tool methodGroupSelection items
]

{ #category : 'selection-method groups' }
ClyFullBrowserContext >> selectedMethodGroups [
	^self actualSelectionFrom: self selectedMethodGroupItems
]

{ #category : 'selection-methods' }
ClyFullBrowserContext >> selectedMethodItems [
	^tool methodSelection items
]

{ #category : 'selection-packages' }
ClyFullBrowserContext >> selectedPackageItems [
	^tool packageSelection items
]

{ #category : 'tool controlling' }
ClyFullBrowserContext >> showPackage: aPackage [
	tool selectPackage: aPackage
]

{ #category : 'tool controlling' }
ClyFullBrowserContext >> showPackage: aPackage tag: tagName [
	tool selectPackage: aPackage andTag: tagName
]

{ #category : 'tool controlling' }
ClyFullBrowserContext >> showProtocol: protocolName [
	tool selectMethodGroupNamed: protocolName
]

{ #category : 'tool controlling' }
ClyFullBrowserContext >> showVariableNamed: varName [
	tool selectVariableNamed: varName
]

{ #category : 'accessing' }
ClyFullBrowserContext >> tool: aBrowser [
	super tool: aBrowser.
	metaLevelScope := aBrowser metaLevelScope
]
