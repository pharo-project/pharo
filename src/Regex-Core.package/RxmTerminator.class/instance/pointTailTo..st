building
pointTailTo: anRxmLink
	"Branch tails are never redirected by the build algorithm.
	Healthy terminators should never receive this."

	RxParser signalCompilationException:
		'internal matcher build error - redirecting terminator tail'