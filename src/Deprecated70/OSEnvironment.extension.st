Extension { #name : #OSEnvironment }

{ #category : #'*Deprecated70' }
OSEnvironment class >> default [

	self
		deprecated: 'Please use #current instead'
		transformWith: '`@receiver default' 
						-> '`@receiver current'.

	^ self current
]
