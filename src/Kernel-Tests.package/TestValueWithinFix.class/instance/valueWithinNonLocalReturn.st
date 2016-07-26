tests
valueWithinNonLocalReturn
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].
