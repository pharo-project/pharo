Class {
	#name : 'IgnoreDeprecationReaction',
	#superclass : 'DeprecationReaction',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
IgnoreDeprecationReaction class >> label [

	^ 'Ignore'
]

{ #category : 'reacting' }
IgnoreDeprecationReaction class >> react: anAutomaticRewriting [
	"Nothing: the deprecation is only recorded in the log."

	
]
