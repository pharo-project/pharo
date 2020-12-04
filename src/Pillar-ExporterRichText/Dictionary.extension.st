Extension { #name : #Dictionary }

{ #category : #'*Pillar-ExporterRichText' }
Dictionary class >> newFromAssociations: anAssocCollection [
	"return a dictionary with the Associations in anAssocCollection.
	If the same key appears twice in anAssocCollection, the last one enumerated will win"
	
	^ anAssocCollection 
		inject: Dictionary new 
		into: [ :dict :assoc | dict add: assoc. dict ]
]
