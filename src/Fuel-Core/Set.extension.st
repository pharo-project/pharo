Extension { #name : #Set }

{ #category : #'*Fuel-Core' }
Set >> addIfNotPresent: anObject ifPresentDo: aBlock [
	"Include anObject as one of the receiver's elements and then value aBlock, but only if there
	is no such element already. Anwser anObject."

	| index |
	index := self scanFor: anObject.
	(array at: index) 
		ifNil: [self atNewIndex: index put: anObject asCollectionElement]
		ifNotNil: [ aBlock value ].
	^ anObject

	"Sets in Pharo 1.2 can't' contain nil, so this implementation is wrong in that version. This is the right one (in Pharo 1.2):

	| index |
	anObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: anObject.
	(array at: index) 
		ifNil: [ self atNewIndex: index put: anObject ]
		ifNotNil: [ aBlock value ].
	^ anObject"
]

{ #category : #'*Fuel-Core' }
Set >> fuelAccept: aGeneralMapper [
	"Since we have subclasses of Set that behave differently, we cannot use the visitSimpleCollection: for all of them."
	^ ((self class == Set) or: [ self class == IdentitySet ] ) 
		ifTrue: [ aGeneralMapper visitSimpleCollection: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper ] 
	
]

{ #category : #'*Fuel-Core' }
Set >> fuelAfterMaterialization [

	"Since for Set and IdentitySet we are recreating the collection with #add: (see FLSimpleCollectionCluster) we do not need to rehash"
	^ ((self class == Set) or: [ self class == IdentitySet ] ) 
		ifFalse: [ self rehash ] 
		ifTrue: [ self ]
]
