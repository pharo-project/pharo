Extension { #name : #StrikeFont }

{ #category : #'*System-Settings-Core' }
StrikeFont >> acceptSettings: aVisitor [
	^ aVisitor visitStrikeFont: self
]
