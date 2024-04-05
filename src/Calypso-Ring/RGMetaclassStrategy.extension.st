Extension { #name : 'RGMetaclassStrategy' }

{ #category : '*Calypso-Ring' }
RGMetaclassStrategy >> definitionString [
	"Answer the receiver's <String> class definition resolving it from the receiver's package information"

	^ self package definitionFor: self
]

{ #category : '*Calypso-Ring' }
RGMetaclassStrategy >> definitionStringFor: aRGPackage [
	"Answer the receiver's definition <String> contained in aRGPackage Monticello snapshot information"

	^ (aRGPackage asMCSnapshot
		classDefinitionNamed: self owner instanceSide name
		ifAbsent: [ self error: 'No class definition found for the recever' ])
			classDefinitionString
]
