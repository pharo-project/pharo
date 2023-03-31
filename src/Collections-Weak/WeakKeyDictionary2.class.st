Class {
	#name : #WeakKeyDictionary2,
	#superclass : #Dictionary,
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #adding }
WeakKeyDictionary2 >> add: anAssociation [

	self error: 'Cancelled API. Cannot add associations directly to a weak key dictionary because it will break invariants. Use #at:put: instead.'
]

{ #category : #accessing }
WeakKeyDictionary2 >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [self atNewIndex: index put: ((WeakKeyAssociation2 key: key value: anObject) container: self)]
		ifNotNil: [assoc value: anObject].
	^ anObject
]

{ #category : #copying }
WeakKeyDictionary2 >> postCopy [
	"Must copy the associations and tell them I (the copy) am their new container"
	array := array collect: [ :assoc |
		         assoc ifNotNil: [
			         assoc copy
				         container: self;
				         yourself ] ]
]
