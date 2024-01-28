Extension { #name : 'RGObject' }

{ #category : '*Calypso-Ring' }
RGObject >> realClass [
	"Answer the <Class> representing the receiver"

	^ self isMeta
		ifTrue: [ (self class environment at: self instanceSide resolvedNameOrNil) class ]
		ifFalse: [ self class environment at: self resolvedNameOrNil ].

]
