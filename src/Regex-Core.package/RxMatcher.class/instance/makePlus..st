private
makePlus: aMatcher
	"Private - Wrap this matcher so that the result would match 1 and more
	occurrences of the matcher."

	| loopback |
	loopback := (RxmBranch new beLoopback)
		next: aMatcher.
	aMatcher pointTailTo: loopback.
	^aMatcher