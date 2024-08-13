Extension { #name : 'FormSet' }

{ #category : '*Morphic-Base' }
FormSet >> asMorph [

	^ ImageMorph withFormSet: self
]
