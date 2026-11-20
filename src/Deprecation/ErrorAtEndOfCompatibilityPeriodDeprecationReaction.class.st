"
I signal an error for a deprecation that reached the last version of its compatibility period (PHEP-0004), when its callers are expected to have migrated, and otherwise only log it: a deprecation still in its period is a legitimate use of a supported API.
"
Class {
	#name : 'ErrorAtEndOfCompatibilityPeriodDeprecationReaction',
	#superclass : 'DeprecationReaction',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'reacting' }
ErrorAtEndOfCompatibilityPeriodDeprecationReaction class >> failsTestsFor: anAutomaticRewriting [

	^ anAutomaticRewriting isAtEndOfCompatibilityPeriod
]

{ #category : 'accessing' }
ErrorAtEndOfCompatibilityPeriodDeprecationReaction class >> label [

	^ 'Error at the end of the compatibility period, Transcript message before'
]

{ #category : 'reacting' }
ErrorAtEndOfCompatibilityPeriodDeprecationReaction class >> react: anAutomaticRewriting [

	(self failsTestsFor: anAutomaticRewriting) ifFalse: [ ^ anAutomaticRewriting logTranscript ].
	anAutomaticRewriting signalError
]
