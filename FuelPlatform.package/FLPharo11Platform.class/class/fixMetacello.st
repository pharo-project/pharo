private-hacks-metacello
fixMetacello
	[ Gofer it
	    squeaksource: 'MetacelloRepository';
	    package: 'ConfigurationOfMetacello';
	    load.
	(Smalltalk at: #ConfigurationOfMetacello) load ]
		on: Deprecation do: [ :exception | exception resume ]