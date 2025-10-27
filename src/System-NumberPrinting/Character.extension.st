Extension { #name : 'Character' }

{ #category : '*System-NumberPrinting' }
Character >> printStringHex [
	"$A printStringHex >>> '41'"

	^ self asInteger printStringBase: 16
]
