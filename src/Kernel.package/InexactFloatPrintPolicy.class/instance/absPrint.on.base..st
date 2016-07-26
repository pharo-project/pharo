printing
absPrint: float on: stream base: base
	"Doube dispatch to the faster but potentially less accurate way to print"

	^ float absPrintInexactlyOn: stream base: base 