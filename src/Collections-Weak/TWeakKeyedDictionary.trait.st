Trait {
	#name : #TWeakKeyedDictionary,
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #adding }
TWeakKeyedDictionary >> add: anAssociation [
	
	"Installs a copy of the association in myself.
	I cannot add the association because they are not shareable between weak key dictionaries, it needs myself as container.
	Moreover, I can only contain weak key associations, and not any association."
	self at: anAssociation key put: anAssociation value
]

{ #category : #copying }
TWeakKeyedDictionary >> array: anArray [

	self explicitRequirement
]

{ #category : #accessing }
TWeakKeyedDictionary >> at: key put: anObject [
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

{ #category : #copying }
TWeakKeyedDictionary >> postCopy [
	"Must copy the associations and tell them I (the copy) am their new container"

	self array: (self array collect: [ :assoc |
			 assoc ifNotNil: [
				 assoc copy
					 container: self;
					 yourself ] ])
]
