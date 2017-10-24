tests - conversion
testAsLowercaseBasicAsciiRange
	| lowercase uppercase |
	lowercase  := 'abcdefghijklmnopqrstuvwxyz123456789'.
	uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789'.
	uppercase withIndexDo: [ :char :index |
		self assert: char asLowercase equals: (lowercase at: index)]