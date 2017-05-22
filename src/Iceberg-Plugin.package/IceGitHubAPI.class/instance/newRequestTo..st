private factory
newRequestTo: aString
	^ ZnClient new 
		in: [ :this | 
			self credentials ifNotNil: [ 
				this 
					username: self credentials username 
					password: self credentials password ] ]; 
		headerAt: 'Accept' put: 'application/vnd.github.v3+json';
		url: 'https://api.github.com/', aString;
		yourself