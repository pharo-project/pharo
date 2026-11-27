Class {
	#name : 'NeverRewriteDeprecation',
	#superclass : 'DeprecationRewriting',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
NeverRewriteDeprecation class >> label [

	^ 'Never'
]

{ #category : 'testing' }
NeverRewriteDeprecation class >> shouldRewrite: anAutomaticRewriting [

	^ false
]
