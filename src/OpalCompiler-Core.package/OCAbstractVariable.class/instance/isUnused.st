read/write usage
isUnused
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil