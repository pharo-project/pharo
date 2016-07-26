printing
absPrint: float on: stream base: base
	"Doube dispatch to the slower but accurate way to print"

	^ float absPrintExactlyOn: stream base: base 