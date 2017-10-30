tests - conversion
testAsUppercaseBasicAsciiRange
	| lowercase uppercase |
	lowercase  := 'abcdefghijklmnopqrstuvwxyz123456789'.
	uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789'.
	lowercase withIndexDo: [ :char :index |
		self assert: char asUppercase equals: (uppercase at: index)]