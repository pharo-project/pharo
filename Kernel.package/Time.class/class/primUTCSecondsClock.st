primitives
primUTCSecondsClock
	"Answer the number of whole seconds ellapsed since Squeak epoch.
	That is since 00:00 on the morning of January 1, 1901 UTC"

	^self primUTCMicrosecondsClock // 1e6