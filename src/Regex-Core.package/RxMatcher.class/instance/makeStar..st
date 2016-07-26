private
makeStar: aMatcher
	"Private - Wrap this matcher so that the result would match 0 and more
	occurrences of the matcher."

	| dummy detour loopback |
	dummy := RxmLink new.
	detour := RxmBranch new
		next: aMatcher;
		alternative: dummy.
	loopback := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: loopback.
	^detour