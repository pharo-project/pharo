"
I represent a class as a ̀FinderResult̀.
"
Class {
	#name : #FinderClassResult,
	#superclass : #FinderResult,
	#category : #'Tool-Finder-Result'
}

{ #category : #action }
FinderClassResult >> browseAction [

	content browse
]

{ #category : #testing }
FinderClassResult >> hasBrowseAction [

	^ true
]

{ #category : #testing }
FinderClassResult >> hasHierarchyAction [

	^ true
]

{ #category : #testing }
FinderClassResult >> hasInheritanceAction [

	^ self hasSelectorParent
]

{ #category : #testing }
FinderClassResult >> hasSelectorParent [

	^ self parent notNil and: [ self parent isSelectorResult ]
]

{ #category : #testing }
FinderClassResult >> hasVersionsAction [

	^ self hasSelectorParent
]

{ #category : #action }
FinderClassResult >> hierarchyAction [

	self hasSelectorParent ifTrue: [
		SystemNavigation new
			browseHierarchy: content
			selector: self parent content.
		^ self ].

	SystemNavigation new browseHierarchy: content
]

{ #category : #action }
FinderClassResult >> inheritanceAction [

	self hasInheritanceAction ifFalse: [ ^ self ].

	SystemNavigation new
		methodHierarchyBrowserForClass: self content
		selector: self parent content
]

{ #category : #testing }
FinderClassResult >> isClassResult [

	^ true
]

{ #category : #action }
FinderClassResult >> versionsAction [

	self hasVersionsAction ifFalse: [ ^ self ].

	Smalltalk tools versionBrowser
		browseVersionsForClass: self content
		selector: self parent content
]
