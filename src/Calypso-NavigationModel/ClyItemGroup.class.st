"
I am a root of hierarchy of groups for different kind of objects.
I provide name and priority for users: 	
	ClyConcreteItemGroup named: 'a group name'.
	ClyConcreteItemGroup named: 'a group name' priority 100.
Priority is used to sort groups when they are represented by sorted query result. By default bigger priority moves group to the end of result list.

When groups are represented as browser items they are able to decorate them with specific properties: 
	aGroup decorateOwnBrowserItem: aBrowserItem

And when group is shown in the browser it can decorate table cells:
	aGroup decorateTableCell: anItemCellMorph of: groupItem
By default I decorate cell with special color when it shows readonly groups.

Any group is readonly by default. To mark editable group you should implement class side method #isEditableGroup by returning true
"
Class {
	#name : #ClyItemGroup,
	#superclass : #Object,
	#instVars : [
		'name',
		'priority'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #testing }
ClyItemGroup class >> checkCalypsoItem: aGroup isSameAs: aGroup2 [
	^(super checkCalypsoItem: aGroup isSameAs: aGroup2)
		or: [ aGroup = aGroup2 ]
]

{ #category : #'browser item preparation' }
ClyItemGroup class >> convertToCalypsoBrowserItem: aGroup [

	^ClyBrowserItem named: aGroup name with: aGroup
]

{ #category : #sorting }
ClyItemGroup class >> defaultSortFunctionForCalypso [
	^ClySortItemGroupFunction ascending
]

{ #category : #testing }
ClyItemGroup class >> isEditableGroup [
	^false
]

{ #category : #'instance creation' }
ClyItemGroup class >> named: aString [
	^self new 
		name: aString
]

{ #category : #'instance creation' }
ClyItemGroup class >> named: aString priority: aNumber [
	^(self named: aString)
			priority: aNumber
		
]

{ #category : #accessing }
ClyItemGroup class >> priority [
	^1
]

{ #category : #accessing }
ClyItemGroup >> calypsoEnvironmentType [
	^self class
]

{ #category : #decoration }
ClyItemGroup >> decorateOwnBrowserItem: myItem [
]

{ #category : #testing }
ClyItemGroup >> isEditableGroup [
	^self class isEditableGroup
]

{ #category : #accessing }
ClyItemGroup >> name [
	^name
]

{ #category : #accessing }
ClyItemGroup >> name: aString [
	name := aString
]

{ #category : #accessing }
ClyItemGroup >> priority [
	^priority ifNil: [ self class priority ]
]

{ #category : #accessing }
ClyItemGroup >> priority: anObject [
	priority := anObject
]
