"
I am a storage of shortcuts. I store and provide facilities to access named and annonimous shortcuts.
"
Class {
	#name : #KMStorage,
	#superclass : #Object,
	#instVars : [
		'namedRegistry',
		'anonymousRegistry'
	],
	#category : #'Keymapping-Core-Base'
}

{ #category : #accessing }
KMStorage >> , aKMStorage [
	^self class new
		addAll: self keymaps;
		addAll: aKMStorage keymaps.
]

{ #category : #accessing }
KMStorage >> add: aKeymap [
	aKeymap hasName
		ifTrue: [ namedRegistry at: aKeymap name put: aKeymap ]
		ifFalse: [ anonymousRegistry add: aKeymap ].
]

{ #category : #accessing }
KMStorage >> addAll: keymaps [
	keymaps do: [ :km | self add: km ].
]

{ #category : #accessing }
KMStorage >> hasKeymapNamed: aKmName [
	^namedRegistry includesKey: aKmName
]

{ #category : #initialization }
KMStorage >> initialize [
	anonymousRegistry := Set new.
	namedRegistry := Dictionary new.
]

{ #category : #accessing }
KMStorage >> keymapForShortcut: aShortcut [

	^ namedRegistry
		detect: [ :keymap | keymap shortcut = aShortcut ]
		ifNone: [ 
			anonymousRegistry
				detect: [ :keymap | keymap shortcut = aShortcut ]
				ifNone: [ nil ] ]
]

{ #category : #accessing }
KMStorage >> keymapNamed: aKmName [
	^namedRegistry at: aKmName
]

{ #category : #accessing }
KMStorage >> keymaps [
	^anonymousRegistry , namedRegistry values
]

{ #category : #accessing }
KMStorage >> remove: aKeymap [
	aKeymap hasName
		ifTrue: [ namedRegistry removeKey: aKeymap name ]
		ifFalse: [ anonymousRegistry remove: aKeymap ].
]

{ #category : #accessing }
KMStorage >> size [
	^self keymaps size
]
