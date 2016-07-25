private
classNamesFor: anEnvironment 
	| classNames allClassNames |
	classNames := IdentitySet new
		addAll: anEnvironment classNames asSet;
		addAll: anEnvironment not classNames;
		yourself.
	allClassNames := universalEnvironment classNames asSortedCollection.
	self assert: classNames asSortedCollection 	= allClassNames.
	self assert: (anEnvironment & anEnvironment not) classNames isEmpty.
	self assert: (anEnvironment | anEnvironment not) classNames asSortedCollection = allClassNames