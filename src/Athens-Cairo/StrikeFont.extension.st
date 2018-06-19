Extension { #name : #StrikeFont }

{ #category : #'*Athens-Cairo' }
StrikeFont >> asAthensFontDescription [
	^ AthensFontDescription new family: self familyName; fontSize: self pointSize.

]
