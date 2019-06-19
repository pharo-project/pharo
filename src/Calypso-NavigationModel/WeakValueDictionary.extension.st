Extension { #name : #WeakValueDictionary }

{ #category : #'*Calypso-NavigationModel' }
WeakValueDictionary >> clyCleanGarbage [

	| firstFound |
	array withIndexDo: [:ass :index | 
		(ass notNil and: [ass value isNil]) ifTrue: [ 
			array at: index put: nil.
			tally := tally - 1.
			firstFound ifNil: [firstFound := index]]].
	
	firstFound ifNotNil: [ 
		self fixCollisionsFrom: firstFound]
]

{ #category : #'*Calypso-NavigationModel' }
WeakValueDictionary >> clyIncludesCleanedKey: key [
	"Answer whether the receiver has a key which value was collected as garbage"
	
	^ (array at: (self scanFor: key)) value == nil
]
