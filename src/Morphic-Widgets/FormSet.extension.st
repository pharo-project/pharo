Extension { #name : 'FormSet' }

{ #category : '*Morphic-Widgets' }
FormSet >> asMorph [

	^ ImageMorph withFormSet: self
]
