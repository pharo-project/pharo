input
readTimezoneOffsetFrom: stream
	"Read and return an optional timezone offset in the form of 
	[+|-]hh[[separator]mm[[separator]ss]] or Z from stream as a duration.
	If there is no offset, return the local offset."

	^self readTimezoneOffsetFrom: stream default: self localOffset