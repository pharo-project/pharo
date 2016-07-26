testing
generateTestFiles
	"self generateTestFiles"
	
	(Integer primesUpTo: 100) do: [ :each | | size |
		size := 1024 * each + each.
		FileStream fileNamed: ('test-', size asString, '.txt') do: [ :stream |
			stream nextPutAll: (self generateTestData: size) ] ]