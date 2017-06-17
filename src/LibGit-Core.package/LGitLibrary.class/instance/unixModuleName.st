accessing platform
unixModuleName
	| pluginDir |
	pluginDir := Smalltalk vm binary parent.
	#('libgit2.so' 'libgit2.so.0')
		detect: [ :each | (pluginDir / each) exists ] 
		ifFound: [ :libName | ^ libName ].

	self error: 'Module not found.'