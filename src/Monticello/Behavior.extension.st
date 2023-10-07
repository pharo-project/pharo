Extension { #name : 'Behavior' }

{ #category : '*Monticello' }
Behavior >> mcType [
	"Answer the symbol that Monticello uses internally to encode layouts"
	^self classLayout class mcTypeSymbol
]
