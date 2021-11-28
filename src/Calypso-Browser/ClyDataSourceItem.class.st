"
I represent item of ClyDataSource.
I am created with owner data source and browser item which I wrap:
	ClyDataSourceItem of: aDataSource value: aBrowserItem
You can access actual object of browser item:
	aDataSourceItem actualObject

You can control expansion
- supportsExpansion
- collapse
- expand
- isExpanded
When I expand I keep children items in childrenDataSource.

Methods to manage position in table: 
- position. It is position of environment item in owner environment content.
- depth. It is depth of environment item in owner environment content.
- globalPosition. It is row index in owner data source table.

Methods to manage tree structure in table:
- parentItem. It is parent data source.
- rootParentItem
- rootDataSource
- childrenItems
- childrenCount
- childrenItemAt: ownerIndex

You can convert me to selection instance with single item: 
- asSelection

You can retrieve item properties:
- getProperty: aPropertyClass 
- getProperty: aPropertyClass ifAbsent: aBlock
- hasProperty: aPropertyClass
- isMarkedWith: aPropertyTagClass
 
Internal Representation and Key Implementation Points.

    Instance Variables
	browserItem:		<ClyBrowserItem>
	ownerDataSource:		<ClyDataSource>
	childrenDataSource:		<ClyDataSource>
"
Class {
	#name : #ClyDataSourceItem,
	#superclass : #Object,
	#instVars : [
		'ownerDataSource',
		'childrenDataSource',
		'browserItem'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #'instance creation' }
ClyDataSourceItem class >> of: aDataSource value: anEnvironmentItem [
	^self new 
		ownerDataSource: aDataSource;
		browserItem: anEnvironmentItem
]

{ #category : #accessing }
ClyDataSourceItem >> actualObject [
	"environmentItem can be nil when actual object was removed. In that case actualObject should be nil too. It is convention of updating items/selection logic"
	^browserItem ifNotNil: [browserItem actualObject]
]

{ #category : #accessing }
ClyDataSourceItem >> actualObject: anObject [
	^browserItem actualObject: anObject
]

{ #category : #converting }
ClyDataSourceItem >> asSelection [
	^self rootDataSource newSelectionWith: {self}
]

{ #category : #testing }
ClyDataSourceItem >> belongsToDataSource: aDataSource [
	^ownerDataSource == aDataSource
]

{ #category : #accessing }
ClyDataSourceItem >> browser [
	^self queryView browser
]

{ #category : #accessing }
ClyDataSourceItem >> browserItem [
	^ browserItem
]

{ #category : #accessing }
ClyDataSourceItem >> browserItem: aBrowserItem [
	browserItem := aBrowserItem
]

{ #category : #accessing }
ClyDataSourceItem >> childrenCount [
	^ownerDataSource countChildrenOf: self
]

{ #category : #accessing }
ClyDataSourceItem >> childrenDataSource [
	^ childrenDataSource
]

{ #category : #accessing }
ClyDataSourceItem >> childrenDataSource: aDataSource [
	childrenDataSource := aDataSource.
	childrenDataSource ifNotNil: [ 
		childrenDataSource parentItem: self.
		childrenDataSource depth: self depth + 1]
]

{ #category : #accessing }
ClyDataSourceItem >> childrenItemAt: ownerIndex [

	^childrenDataSource elementAt: ownerIndex - self position
]

{ #category : #accessing }
ClyDataSourceItem >> childrenItems [

	^childrenDataSource allElements
]

{ #category : #controlling }
ClyDataSourceItem >> collapse [
	ownerDataSource collapse: self
]

{ #category : #copying }
ClyDataSourceItem >> copy [

	| copy |
	copy := super copy.
	copy browserItem: browserItem copy.
	^copy
]

{ #category : #copying }
ClyDataSourceItem >> copyForBrowserStateSnapshot [
	^self copyForBrowserStateSnapshotOf: ownerDataSource copyForBrowserStateSnapshot
]

{ #category : #copying }
ClyDataSourceItem >> copyForBrowserStateSnapshotOf: rootDataSourceSnapshot [
	| copy ownerDataSourceSnapshot |
	ownerDataSourceSnapshot := rootDataSourceSnapshot 
		findDataSourceSameAs: ownerDataSource 
		ifNone: [ self error: 'should not happen' ].	
	copy := self copy.
	copy ownerDataSource: ownerDataSourceSnapshot.
	childrenDataSource ifNotNil: [ 
		copy childrenDataSource: childrenDataSource copyForBrowserStateSnapshot].
	^copy
]

{ #category : #accessing }
ClyDataSourceItem >> depth [
	^ownerDataSource depth + browserItem depth
]

{ #category : #controlling }
ClyDataSourceItem >> expand [
	self isExpanded ifTrue: [ ^self ].
	
	ownerDataSource expand: self
]

{ #category : #controlling }
ClyDataSourceItem >> expandChildren [
	
	self childrenDataSource: (ownerDataSource retrieveChildrenOf: self)
]

{ #category : #accessing }
ClyDataSourceItem >> findParentItemOf: itemTypeClass [
	| parent |
	parent := self parentItem ifNil: [ ^nil ].
	(parent isBasedOnItemType: itemTypeClass) ifTrue: [ ^parent ].
	
	^parent findParentItemOf: itemTypeClass
]

{ #category : #accessing }
ClyDataSourceItem >> getProperty: anEnvironmentPropertyClass [
	^browserItem getProperty: anEnvironmentPropertyClass
]

{ #category : #accessing }
ClyDataSourceItem >> getProperty: anEnvironmentPropertyClass ifAbsent: absentBlock [
	^browserItem getProperty: anEnvironmentPropertyClass ifAbsent: absentBlock
]

{ #category : #accessing }
ClyDataSourceItem >> globalPosition [
	^ownerDataSource globalPositionOf: self
]

{ #category : #testing }
ClyDataSourceItem >> hasChildAt: ownerDataSourceIndex [

	^ownerDataSourceIndex > self position and: [
		ownerDataSourceIndex <= (self position + self childrenCount)]
]

{ #category : #accessing }
ClyDataSourceItem >> hasChildren [
	^ownerDataSource doesItemHaveChildren: self
]

{ #category : #testing }
ClyDataSourceItem >> hasProperty: propertyClass [
	^browserItem hasProperty: propertyClass
]

{ #category : #testing }
ClyDataSourceItem >> includesActualObject: anObject [

	^ self browserItem includesActualObject: anObject
]

{ #category : #testing }
ClyDataSourceItem >> isAfter: ownerIndex [

	^ownerIndex <= self position
]

{ #category : #testing }
ClyDataSourceItem >> isBasedOnItemType: aClass [
	^browserItem representsItemOfType: aClass
]

{ #category : #testing }
ClyDataSourceItem >> isEqualTo: anotherItem [
	^browserItem isEqualTo: anotherItem browserItem
]

{ #category : #testing }
ClyDataSourceItem >> isExpanded [
	^ownerDataSource isExpanded: self
]

{ #category : #testing }
ClyDataSourceItem >> isMarkedWith: propertyClass [
	^browserItem isMarkedWith: propertyClass
]

{ #category : #testing }
ClyDataSourceItem >> isRemoved [
	^browserItem isNil 
		or: [ ownerDataSource isParentRemoved 
				or: [ ownerDataSource isParentCollapsed] ]
]

{ #category : #testing }
ClyDataSourceItem >> isRoot [
	^ownerDataSource isRoot & browserItem isRoot
]

{ #category : #testing }
ClyDataSourceItem >> isSameAs: anotherItem [
	^browserItem isSameAs: anotherItem browserItem
]

{ #category : #testing }
ClyDataSourceItem >> isSameKindAs: anotherItem [
	^self type = anotherItem type
]

{ #category : #accessing }
ClyDataSourceItem >> name [
	^browserItem name
]

{ #category : #accessing }
ClyDataSourceItem >> ownerDataSource [
	^ ownerDataSource
]

{ #category : #accessing }
ClyDataSourceItem >> ownerDataSource: anObject [
	ownerDataSource := anObject
]

{ #category : #accessing }
ClyDataSourceItem >> parentItem [
	^ ownerDataSource isRoot 
		ifTrue: [ nil ]
		ifFalse: [ ownerDataSource parentItem]
]

{ #category : #accessing }
ClyDataSourceItem >> position [
	^browserItem position
]

{ #category : #printing }
ClyDataSourceItem >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	browserItem 
		ifNil: [ nil printOn: aStream ]
		ifNotNil: [ aStream nextPutAll: browserItem name ].
	aStream nextPut: $).
]

{ #category : #accessing }
ClyDataSourceItem >> queryView [
	^ownerDataSource queryView
]

{ #category : #accessing }
ClyDataSourceItem >> rootDataSource [
	^ ownerDataSource isRoot 
		ifTrue: [ ownerDataSource ]
		ifFalse: [ ownerDataSource parentItem rootDataSource ]
]

{ #category : #accessing }
ClyDataSourceItem >> rootParentItem [
	^ ownerDataSource isRoot 
		ifTrue: [ self ]
		ifFalse: [ self parentItem rootParentItem ]
]

{ #category : #testing }
ClyDataSourceItem >> supportsExpansion [
	^ownerDataSource definesChildren
]

{ #category : #accessing }
ClyDataSourceItem >> systemDefinition [
	^browserItem systemDefinition
]

{ #category : #accessing }
ClyDataSourceItem >> type [
	^browserItem type
]

{ #category : #accessing }
ClyDataSourceItem >> updateItemWith: updatedEnvironmentItem [
	browserItem := updatedEnvironmentItem
]
