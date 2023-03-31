"
I am an identity dictionary holding only weakly on my keys.
When one of my keys is garbage collected, the key->value association is removed from the dictionary.

Internally I use WeakKeyAssociation objects that know myself as their container.
WeakKeyAssociation are implemented as ephemerons and get mourned when their key is only known from itself.
On mourn, the association asks the container to remove itself.
"
Class {
	#name : #WeakIdentityKeyDictionary,
	#superclass : #IdentityDictionary,
	#traits : 'TWeakKeyedDictionary',
	#classTraits : 'TWeakKeyedDictionary classTrait',
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #copying }
WeakIdentityKeyDictionary >> array: anArray [

	array := anArray
]
