private
makeOptional: aMatcher
	"Private - Wrap this matcher so that the result would match 0 or 1
	occurrences of the matcher."

	| dummy branch |
	dummy := RxmLink new.
	branch := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: dummy.
	^branch