tests
testMilliSeconds
	#(
	"argument (milliseconds)        seconds nanoseconds"
	(5                                                      0                       5000000)
	(1005                                           1                       5000000)
	(-5                                                     0                       -5000000)
	(-1005                                          -1                      -5000000)
	(1234567                                        1234            567000000)
	(-1234567                                       -1234           -567000000))
		do: [ :each |
			| duration |
			duration := Duration milliSeconds: each first.
			self assert: duration asSeconds = each second.
			self assert: duration nanoSeconds = each third ]