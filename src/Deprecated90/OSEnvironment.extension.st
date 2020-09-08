Extension { #name : #OSEnvironment }

{ #category : #'*Deprecated90' }
OSEnvironment >> getEnv: aKey [
	"Gets the value of an environment variable called `aKey`.
	Returns nil if not found."
	self
		deprecated: 'Use #at:ifAbsent: instead'
		on: '16 January 2019'
		in: #Pharo7
		transformWith: '`@receiver getEnv: `@arg' 
						-> '`@receiver at: `@arg ifAbsent: [ nil ]'.

 	^ self at: aKey ifAbsent: [ nil ]
]
