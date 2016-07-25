printing
printOn: aStream delimiter: delimString
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c' "

	self do: [:elem | aStream print: elem] separatedBy: [aStream print: delimString]
		