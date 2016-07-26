accessing
errorTestSuites
	^ errorList
		collect: [ :each | 
			self classForTestSuite new
				in: [ :suite | 
							suite
								addTest: each;
								name: (self label: 'Error' forSuite: suite) ];
				yourself ]