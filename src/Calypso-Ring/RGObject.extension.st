Extension { #name : 'RGObject' }

{ #category : '*Calypso-Ring' }
RGObject >> realClass [
	"Answer the <Class> representing the receiver"

	^ self class environment at: self resolvedNameOrNil
]
