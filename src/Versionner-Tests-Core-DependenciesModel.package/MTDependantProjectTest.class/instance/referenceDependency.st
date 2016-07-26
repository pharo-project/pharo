private
referenceDependency
	| version |
	
	version := MTVersion new
					name: '1.0';
					yourself.
					
	^ (MTDependantProject newNamed: 'LibXYZ')
			configurationClass: self class;
			repositories: #('http://smalltalkhub.com/mc/dummy/XYZ/main', 'http://github.com/XYZ');
			displayName: 'XYZ';
			parent: nil;
			version: version;
			yourself.