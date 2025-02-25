"
I am an identity dictionary holding only weakly on my keys.
When one of my keys is garbage collected, the key->value association is removed from the dictionary.

Internally I use WeakKeyAssociation objects that know myself as their container.
WeakKeyAssociation are implemented as ephemerons and get mourned when their key is only known from itself.
On mourn, the association asks the container to remove itself.
"
Class {
	#name : 'WeakIdentityKeyDictionary',
	#superclass : 'IdentityDictionary',
	#category : 'Collections-Weak-Dictionaries',
	#package : 'Collections-Weak',
	#tag : 'Dictionaries'
}

{ #category : 'adding' }
WeakIdentityKeyDictionary >> add: anAssociation [
	
	"Installs a copy of the association in myself.
	I cannot add the association because they are not shareable between weak key dictionaries, it needs myself as container.
	Moreover, I can only contain weak key associations, and not any association."
	self at: anAssociation key put: anAssociation value.

	"Return a copy of the association.
	This is not the real association in the weak dictionary"
	^ anAssociation
]

{ #category : 'accessing' }
WeakIdentityKeyDictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := self array at: index.
	assoc
		ifNil: [self atNewIndex: index put: ((WeakKeyAssociation key: key value: anObject) container: self)]
		ifNotNil: [assoc value: anObject].
	^ anObject
]

{ #category : 'copying' }
WeakIdentityKeyDictionary >> postCopy [
	"Must copy the associations and tell them I (the copy) am their new container"

	array := self array collect: [ :assoc |
		         assoc ifNotNil: [
			         assoc copy
				         container: self;
				         yourself ] ]
]
