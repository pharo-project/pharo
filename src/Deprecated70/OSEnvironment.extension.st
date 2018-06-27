Extension { #name : #OSEnvironment }

{ #category : #'*Deprecated70' }
OSEnvironment class >> default [

	self deprecated: 'Use #current'.
	^ self current
]
