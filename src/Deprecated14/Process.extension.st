Extension { #name : 'Process' }

{ #category : '*Deprecated14' }
Process >> isActiveProcess [
	"A process is #active if it is currently executing.
	There can be only one such process, held in a slot on the Processor."

	self
		deprecated: 'Please use #isActive instead'
		transformWith: '`@receiver isActiveProcess' -> '`@receiver isActive'.
	^ self isActive
]
