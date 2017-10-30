tests - input
testReadFromWithNanos
	"self debug: #testReadFromWithNanos"
	
	#('4:02:47.5 am'
	'4:02:55.521 pm'
		"there is a bug with 520 instead of 521"
	
	) do: [:each |
			|  t |
			t := self timeClass readFrom: each readStream.
			self assert: t printString = each]
