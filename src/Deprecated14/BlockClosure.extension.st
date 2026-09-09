Extension { #name : 'BlockClosure' }

{ #category : '*Deprecated14' }
BlockClosure >> durationToRun [
	"Answer the duration taken to execute this block."

	self
		deprecated: 'Use #timeToRun instead'
		transformWith: '`@x durationToRun' -> '`@x timeToRun'.
	^ self timeToRun
]
