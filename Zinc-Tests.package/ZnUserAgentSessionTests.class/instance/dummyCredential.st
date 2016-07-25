accessing
dummyCredential
	^ZnBasicCredential new 
		username: 'test'; 
		password: 'foobar';
		realm: 'dummy';
		domain: 'test.com';
		yourself