printing
asStringOn: aStream delimiter: delimString
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'
	Uses #asString instead of #print:."

	self do: [:elem | aStream nextPutAll: elem asString]
		separatedBy: [aStream nextPutAll: delimString]