private
buildPrefixMapping
	(prefixMapping := Dictionary new)
		at: 'new' put: 'instance creation';
		at: 'from' put: 'instance creation';
		at: 'initialize' put: 'initialization';
		at: 'set' put: 'initialization';
		at: 'at' put: 'accessing';
		at: 'first' put: 'accessing';
		at: 'last' put: 'accessing';
		at: 'total' put: 'accessing';
		at: 'max' put: 'accessing';
		at: 'maximum' put: 'accessing';
		at: 'min' put: 'accessing';
		at: 'minimum' put: 'accessing';
		at: 'add' put: 'adding';
		at: 'remove' put: 'removing';
		at: 'is' put: 'testing';
		at: 'has' put: 'testing';
		at: 'includes' put: 'testing';
		at: 'accepts' put: 'testing';
		at: 'can' put: 'testing';
		at: 'matches' put: 'testing';
		at: 'needs' put: 'testing';
		at: 'requires' put: 'testing';
		at: 'print' put: 'printing';
		at: 'copy' put: 'copying';
		at: 'as' put: 'converting';
		at: 'test' put: 'tests';
		at: 'should' put: 'asserting';
		at: 'shouldnt' put: 'asserting';
		at: 'assert' put: 'asserting';
		at: 'deny' put: 'asserting';
		at: 'bench' put: 'benchmarking';
		at: 'benchmark' put: 'benchmarking';
		at: 'accept' put: 'visiting';
		at: 'visit' put: 'visiting';
		at: 'signal' put: 'signalling';
		at: 'parse' put: 'parsing';
		at: 'compile' put: 'compiling';
		at: 'sort' put: 'sorting';
		at: 'format' put: 'formatting';
		at: 'write' put: 'writing'