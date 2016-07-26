instance creation
now 
	"Answer the current date and time expressed in local time.
	[ 10000 timesRepeat: [ self now. ] ] timeToRun / 10000.0 . "

	| nanoTicks |
	nanoTicks := self clock microsecondClockValue * 1e3.
	^ self basicNew
		setJdn: SqueakEpoch 
		seconds: 0
		nano: nanoTicks
		offset: self localOffset