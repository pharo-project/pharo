accessing platform
macModuleName
	| pluginDir |
	pluginDir := Smalltalk vm binary parent / 'Plugins'.
	#('libgit2.dylib' 'libgit2.0.dylib')
		detect: [ :each | (pluginDir / each) exists ] 
		ifFound: [ :libName | ^ libName ].

	self error: 'Module not found.'