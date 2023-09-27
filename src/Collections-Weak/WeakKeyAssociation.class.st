"
I am an association holding only weakly on my key and knowing my container.
I am implemented as an ephemeron that removes itself from my container on mourning.

Since I know my container, I am not shareable between different dictionaries.

"
Class {
	#name : 'WeakKeyAssociation',
	#superclass : 'Association',
	#type : 'ephemeron',
	#instVars : [
		'container'
	],
	#category : 'Collections-Weak-Dictionaries',
	#package : 'Collections-Weak',
	#tag : 'Dictionaries'
}

{ #category : 'accessing' }
WeakKeyAssociation >> container [

	^ container
]

{ #category : 'accessing' }
WeakKeyAssociation >> container: anObject [

	container := anObject
]

{ #category : 'finalization' }
WeakKeyAssociation >> mourn [

	container removeKey: key ifAbsent: [
		"Do Nothing. Probably somebody removed the key while I was in the mourn queue" ]
]
