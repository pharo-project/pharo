Class {
	#name : 'ErrorDeprecationReaction',
	#superclass : 'DeprecationReaction',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
ErrorDeprecationReaction class >> label [

	^ 'Error'
]

{ #category : 'reacting' }
ErrorDeprecationReaction class >> react: anAutomaticRewriting [

	anAutomaticRewriting signalError
]
