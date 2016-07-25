building
buildClassesSearch
	
	^ SearchMorph new
		model: self;
		updateSelector: #classSearchUpdate:;
		ghostText: 'TestCaseA|TestCaseB';
		yourself