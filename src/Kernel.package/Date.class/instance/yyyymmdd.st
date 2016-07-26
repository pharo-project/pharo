printing
yyyymmdd
 	"Format the date in ISO 8601 standard like '2002-10-22'
	The result is of fixed size 10 characters long.."
 
 	^ String new: 10 streamContents: [ :aStream |
		self printOn: aStream format: #(3 2 1 $- 1 1 2) ]