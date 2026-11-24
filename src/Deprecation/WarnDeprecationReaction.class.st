Class {
	#name : 'WarnDeprecationReaction',
	#superclass : 'DeprecationReaction',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
WarnDeprecationReaction class >> label [

	^ 'Transcript message and blocking dialog'
]

{ #category : 'reacting' }
WarnDeprecationReaction class >> react: anAutomaticRewriting [

	anAutomaticRewriting
		logTranscript;
		warn
]
