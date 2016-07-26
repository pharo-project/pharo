code generation (closures)
index: anInteger
	"For renumbering temps in the closure compiler."
	index := anInteger.
	code := self code: index type: LdTempType