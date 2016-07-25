printing
hhmm24
 	"Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm), of exactly 4 digits"
 
 	^ String new: 4 streamContents: [ :aStream | 
		self hour printOn: aStream base: 10 length: 2 padded: true.
		self minute printOn: aStream base: 10 length: 2 padded: true ]