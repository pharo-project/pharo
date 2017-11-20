"
luggableListItemWrapper makes it more easy for clients to use hierarchical lists. Rather than having to write a subclass of ListItemWrapper, a PluggableListItemWrapper can be used to provide the appropriate information straight from the model:
	string - an explicit string representation (contrary to the 'item' which contains any kind of object)
	getStringSelector - a message invoked to retrieve the sting representation of its item dynamically from its model (when a constant representation is undesirable)
	hasContentsSelector - a message invoked in the model to answer whether the item has any children or not.
	getContentsSelector - a message invoked in the model to retrieve the contents for its item.

All callback selectors can have zero, one or two arguments with the item and the wrapper as first and second argument.
"
Class {
	#name : #PluggableListItemWrapper,
	#superclass : #ListItemWrapper,
	#instVars : [
		'string',
		'getContentsSelector',
		'getStringSelector',
		'hasContentsSelector'
	],
	#category : #Morphic-Widgets-List
}

{ #category : #accessing }
PluggableListItemWrapper >> asString [
	string ifNotNil:[^string].
	getStringSelector ifNil:[^super asString].
	^self sendToModel: getStringSelector

]

{ #category : #accessing }
PluggableListItemWrapper >> contents [
	getContentsSelector ifNil:[^#()].
	^self sendToModel: getContentsSelector.
]

{ #category : #accessing }
PluggableListItemWrapper >> getContentsSelector [
	^getContentsSelector
]

{ #category : #accessing }
PluggableListItemWrapper >> getContentsSelector: aSymbol [
	self validateSelector: aSymbol.
	getContentsSelector := aSymbol.
]

{ #category : #accessing }
PluggableListItemWrapper >> getStringSelector [
	^getStringSelector
]

{ #category : #accessing }
PluggableListItemWrapper >> getStringSelector: aSymbol [
	self validateSelector: aSymbol.
	getStringSelector := aSymbol.
]

{ #category : #accessing }
PluggableListItemWrapper >> hasContents [
	hasContentsSelector ifNil:[^super hasContents].
	^self sendToModel: hasContentsSelector

]

{ #category : #accessing }
PluggableListItemWrapper >> hasContentsSelector [
	^hasContentsSelector
]

{ #category : #accessing }
PluggableListItemWrapper >> hasContentsSelector: aSymbol [
	self validateSelector: aSymbol.
	hasContentsSelector := aSymbol.
]

{ #category : #printing }
PluggableListItemWrapper >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut:$(; nextPutAll: self asString; nextPut:$).
]

{ #category : #private }
PluggableListItemWrapper >> sendToModel: aSelector [
	aSelector numArgs = 0 
		ifTrue:[^model perform: aSelector].
	aSelector numArgs = 1 
		ifTrue:[^model perform: aSelector with: item].
	aSelector numArgs = 2 
		ifTrue:[^model perform: aSelector with: item with: self].
]

{ #category : #accessing }
PluggableListItemWrapper >> string [
	^string
]

{ #category : #accessing }
PluggableListItemWrapper >> string: aString [
	string := aString
]

{ #category : #private }
PluggableListItemWrapper >> validateSelector: aSymbol [
	(aSymbol numArgs between: 0 and: 2) ifFalse:[^self error: 'Invalid pluggable selector'].
]
