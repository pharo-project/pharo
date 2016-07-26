initialization
buildPrefixDictionary
	prefixMapping := Dictionary new.
	prefixMapping 
		at: 'test' put: 'tests';
	 	at: 'bench' put: 'benchmarking';
	 	at: 'copy' put: 'copying';
		at: 'initialize' put: 'initialization';
		at: 'accept' put: 'visitor';
		at: 'visit' put: 'visitor';
		at: 'signal' put: 'signalling';
		at: 'parse' put: 'parsing';
		at: 'add' put: 'adding';
		at: 'is' put: 'testing';
		at: 'as' put: 'converting';
		at: 'new' put: 'instance creation'.