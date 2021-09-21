Extension { #name : #ObjectLayout }

{ #category : #'*Monticello' }
ObjectLayout class >> mcTypeSymbol [
	"return the symbol that Monticello uses to encode the layout"
	^self name asSymbol
]
