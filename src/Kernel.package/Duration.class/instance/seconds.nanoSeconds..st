private
seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + NanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - NanosInSecond ]
