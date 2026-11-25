Class {
	#name : 'LogDeprecationReaction',
	#superclass : 'DeprecationReaction',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
LogDeprecationReaction class >> label [

	^ 'Transcript message'
]

{ #category : 'reacting' }
LogDeprecationReaction class >> react: anAutomaticRewriting [

	anAutomaticRewriting logTranscript
]
