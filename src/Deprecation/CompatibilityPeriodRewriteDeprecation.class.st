Class {
	#name : 'CompatibilityPeriodRewriteDeprecation',
	#superclass : 'DeprecationRewriting',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
CompatibilityPeriodRewriteDeprecation class >> label [

	^ 'At the end of the compatibility period (PHEP-0004)'
]

{ #category : 'testing' }
CompatibilityPeriodRewriteDeprecation class >> shouldRewrite: anAutomaticRewriting [
	"PHEP-0004: only a deprecation that states the Pharo version that introduced it, in the last version of its compatibility period or later."

	^ anAutomaticRewriting isAtEndOfCompatibilityPeriod
]
