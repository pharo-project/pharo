Extension { #name : 'Form' }

{ #category : '*Text-Core' }
Form >> asText [
	"I return a text with myself embedded - Similar to Morph>>asText"
	
	^ (String value: 1) asText addAttribute: (TextAnchor new anchoredMorph: self)
]
