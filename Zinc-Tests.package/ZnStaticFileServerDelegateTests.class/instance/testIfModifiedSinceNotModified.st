tests
testIfModifiedSinceNotModified
	self withServerDo: [ :server | | client |
		(client := ZnClient new) 
			beOneShot;
			url: server localUrl;
			addPath: #('local-files' 'small.html');
			"We have to cheat a little bit since our #setUp just created the file,
			it does not alter the semantics of what we try to test"
			setIfModifiedSince: (Date today addDays: 2);
			get.
		self assert: client response isNotModified.
		self deny: client response hasEntity ]