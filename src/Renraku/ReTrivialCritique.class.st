"
I am a trivial tritic that does not provide any more information other than my target and rule
"
Class {
	#name : #ReTrivialCritique,
	#superclass : #ReAbstractCritique,
	#category : #Renraku-Critiques
}

{ #category : #accessing }
ReTrivialCritique >> change [
	^ self shouldNotImplement 
]

{ #category : #testing }
ReTrivialCritique >> providesChange [
	
	^ false
]
