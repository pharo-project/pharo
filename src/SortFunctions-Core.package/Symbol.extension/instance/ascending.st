*SortFunctions-Core-converting
ascending
	"Return a SortFunction around the receiver, where the receiver will be used as a unary message to send to both a and b during sorting, and then the result of said send will be collated in ascending order using the <=> method."
	"#('abc' 'de' 'fghi') sorted: #size ascending >>> #('de' 'abc' 'fghi')"

	^self asSortFunction beAscending 