private
basicSize: anEntry 
	"Returns the basic size of anEntry"
	self 
		deprecated: 'Use basicSizeOf: instead'
		on: 	'3 May 2016' 
		in: 'Pharo-5.0-50746-'.
	^ self basicSizeOf: anEntry 