printing
asStringWithCommas
	"123456789 asStringWithCommas"
	"-123456789 asStringWithCommas"
	^ String streamContents: [:stream | 
		self printWithCommasOn: stream ]