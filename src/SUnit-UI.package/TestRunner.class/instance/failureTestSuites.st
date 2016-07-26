accessing
failureTestSuites
	^ failedList
		collect: [ :each | 
			self classForTestSuite new
				in: [ :suite | 
							suite
								addTest: each;
								name: (self label: 'Failure' forSuite: suite) ];
				yourself ]