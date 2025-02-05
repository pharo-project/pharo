Class {
	#name : 'SycSuperclassImplementorsCommand',
	#superclass : 'SycMessageCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> applyResultInContext: a [
]

{ #category : 'accessing' }
SycSuperclassImplementorsCommand >> defaultMenuIconName [
	^#edit
]

{ #category : 'accessing' }
SycSuperclassImplementorsCommand >> defaultMenuItemName [

	^ 'Hierarchical implementors'
]

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> execute [

	Smalltalk tools messageList browse: self searchInTheWholeHierarchy.
]

{ #category : 'execution' }
SycSuperclassImplementorsCommand >> searchInTheWholeHierarchy [

	| mthImplementors |
	mthImplementors := OrderedCollection new.
	self selectedClass withAllSuperAndSubclasses do: [ : class | 
		(class includesSelector: self selectedSelector) 
			ifTrue: [ mthImplementors add: class >> self selectedSelector ].
		(class class includesSelector: self selectedSelector)
			ifTrue: [ mthImplementors add: class class >> self selectedSelector ] ].
	^ mthImplementors
]
