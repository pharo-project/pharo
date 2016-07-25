tests - converting
testWithNoLineLongerThan
	"self run: #testWithNoLineLongerThan"

	self assert: ('Hello World' withNoLineLongerThan: 5) = ('Hello' , String cr , 'World').
	String crlf withNoLineLongerThan: 5.
	'Hello' , String cr , String cr , 'World' withNoLineLongerThan: 5.
	self
		assert:
			('Sample text to line break at some point' withNoLineLongerThan: 28)
				= ('Sample text to line break at' , String cr , 'some point').
	self
		assert:
			('Sample text' , String cr , 'to line break' , String cr , 'at some point' withNoLineLongerThan: 28)
				= ('Sample text' , String cr , 'to line break' , String cr , 'at some point')