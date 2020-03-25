Class {
	#name : #SpTreeMultipleSelectionMode,
	#superclass : #SpAbstractTreeSelectionMode,
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #testing }
SpTreeMultipleSelectionMode >> includesItem: anItem [
	^ self selectedItems includes: anItem
]

{ #category : #testing }
SpTreeMultipleSelectionMode >> includesItems: anItemList [
	^ self selectedItems includesAll: anItemList
]

{ #category : #testing }
SpTreeMultipleSelectionMode >> includesPath: aPath [
	^ selection includes: aPath
]

{ #category : #testing }
SpTreeMultipleSelectionMode >> includesPaths: aListOfPaths [
	^ selection includesAll: aListOfPaths
]

{ #category : #testing }
SpTreeMultipleSelectionMode >> isMultipleSelection [

	^ true
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> selectPath: aPath [ 
	"Check comment in my superclass to know how to use selectPath:"
	
	aPath ifEmpty: [ ^ self unselectAll ].
	(selection includes: aPath) ifTrue: [ ^ self ].
	presenter itemAtPath: aPath ifAbsent: [ ^ self ].
	
	selection := selection copyWith: aPath.
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> selectPaths: pathArray [
	(pathArray isEmpty 
		or: [ 
			pathArray size = 1 
			and: [ pathArray first isEmpty] ]) 
		ifTrue: [ ^ self unselectAll ].

	pathArray
		do: [ :path | presenter itemAtPath: path ifAbsent: [ ^ self ] ].
	selection = pathArray ifTrue: [ ^ self ].
	
	selection := pathArray
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedItem [
	^ self selectedPaths 
		ifEmpty: [ #() ]
		ifNotEmpty: [ :paths | presenter itemAtPath: paths first  ]
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedItems [
	selection ifEmpty: [ ^ #() ].
	^ selection collect: [ :path | presenter itemAtPath: path ]
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedPaths [
	^ selection
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> unselectPath: aPath [
	self selectPaths: (self selectedPaths copyWithout: aPath)
]
