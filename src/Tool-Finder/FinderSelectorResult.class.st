"
I represent a selector as a ̀FinderResult̀.
"
Class {
	#name : #FinderSelectorResult,
	#superclass : #FinderResult,
	#category : #'Tool-Finder-Result'
}

{ #category : #action }
FinderSelectorResult >> browseAction [

	self hasBrowseAction ifFalse: [ ^ self ].

	Smalltalk tools browser openOnClass: self parent content selector: self content
]

{ #category : #testing }
FinderSelectorResult >> hasBrowseAction [

	^ self hasClassParent
]

{ #category : #testing }
FinderSelectorResult >> hasClassParent [

	^ self parent notNil and: [ self parent isClassResult ]
]

{ #category : #testing }
FinderSelectorResult >> hasHierarchyAction [

	^ self hasClassParent
]

{ #category : #testing }
FinderSelectorResult >> hasImplementersAction [

	^ true
]

{ #category : #testing }
FinderSelectorResult >> hasInheritanceAction [

	^ self hasClassParent
]

{ #category : #testing }
FinderSelectorResult >> hasSendersAction [

	^ true
]

{ #category : #testing }
FinderSelectorResult >> hasVersionsAction [

	^ self parent notNil and: [ self parent isClassResult ]
]

{ #category : #action }
FinderSelectorResult >> hierarchyAction [

	self hasHierarchyAction ifFalse: [ ^ self ].

	SystemNavigation new
		browseHierarchy: self parent content
		selector: self content
]

{ #category : #action }
FinderSelectorResult >> implementersAction [

	SystemNavigation new browseAllImplementorsOf: content
]

{ #category : #action }
FinderSelectorResult >> inheritanceAction [

	self hasInheritanceAction ifFalse: [ ^ self ].

	SystemNavigation new
		methodHierarchyBrowserForClass: self parent content
		selector: self content
]

{ #category : #testing }
FinderSelectorResult >> isSelectorResult [

	^ true
]

{ #category : #action }
FinderSelectorResult >> sendersAction [

	SystemNavigation new
		browseSendersOf: content
		name: 'Senders of ' , content asString
		autoSelect: content asString
]

{ #category : #action }
FinderSelectorResult >> versionsAction [

	Smalltalk tools versionBrowser
		browseVersionsForClass: self parent content
		selector: self content
]
