Class {
	#name : 'AlwaysRewriteDeprecation',
	#superclass : 'DeprecationRewriting',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
AlwaysRewriteDeprecation class >> label [

	^ 'Always, as soon as the deprecated method runs'
]

{ #category : 'testing' }
AlwaysRewriteDeprecation class >> shouldRewrite: anAutomaticRewriting [

	^ true
]
