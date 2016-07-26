*GT-Inspector-Examples-Core
gtExamplesFinder
	^ GTExampleFinder new
		source: self gtExamplesSource;
		provider: self gtExamplesProvider;
		subjects: self gtExamplesSubjects;
		yourself