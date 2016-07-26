signals
kill: aPid signal: aSignal
	"Sends aSignal to aPid"
	| answer | 
	answer := self primitiveKill: aPid signal: aSignal.
	(answer = -1) ifTrue: [ 
		self perror: 'kill()'.
	].
	(answer = 0) 
		ifTrue: [ ^ answer ]
		ifFalse: [ self error: 'Unkown answer for kill()' ]