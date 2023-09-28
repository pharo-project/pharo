Extension { #name : 'ObjectLayout' }

{ #category : '*Monticello' }
ObjectLayout class >> layoutForType: typeSymbol [
	"used to get the layout for a Monticello internal layout symbol"
	^self allSubclasses 
		detect: [ :class | class mcTypeSymbol = typeSymbol ] 
		ifNone: [ Error signal: 'Invalid layout type: ', typeSymbol asString ]
]

{ #category : '*Monticello' }
ObjectLayout class >> mcTypeSymbol [
	"return the symbol that Monticello uses to encode the layout"
	^self name asSymbol
]
