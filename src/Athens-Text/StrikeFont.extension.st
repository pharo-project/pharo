Extension { #name : #StrikeFont }

{ #category : #'*Athens-Text' }
StrikeFont >> asFreetypeFont [

		self error: 'not supported yet. and ever'
]

{ #category : #'*Athens-Text' }
StrikeFont >> getPreciseAscent [
	^ self ascent
]

{ #category : #'*Athens-Text' }
StrikeFont >> glyphRendererOn: anAthensSurface [

	^ anAthensSurface getStrikeFontRendererFor: self

]
