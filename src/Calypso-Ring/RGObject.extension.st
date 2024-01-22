Extension { #name : 'RGObject' }

{ #category : '*Calypso-Ring' }
RGObject >> realClass [
	"Answer the <Class> representing the receiver"

	^ Smalltalk at: self resolvedNameOrNil
]
