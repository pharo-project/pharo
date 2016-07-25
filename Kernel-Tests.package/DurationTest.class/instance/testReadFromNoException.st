tests
testReadFromNoException
	"self debug: #testReadFromNoException"

	#(	'0:00:00:00' 
		'0:00:00:00.000000001' 
		'0:00:00:00.999999999' 
		'0:00:00:00.100000000' 
		'0:00:00:00.10' 
		'0:00:00:00.1' 
		'0:00:00:01' 
		'0:12:45:45' 
		'1:00:00:00' 
		'365:00:00:00' 
		'-7:09:12:06.10' 
		'+0:01:02:55' 
		'+0:01:02:3') do: [:each | each asDuration printString = each]