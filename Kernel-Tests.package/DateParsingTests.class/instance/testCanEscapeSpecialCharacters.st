tests
testCanEscapeSpecialCharacters
	"Checks that reserved pattern characters $d $m and $y can be escaped in patterns"
	self assertReading: '4d\2m\2345y' as: 'd\d\\m\m\\y\y' equals: (Date year: 2345 month: 2 day: 4)
		
	