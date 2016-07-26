running
setUp
	| deprClass |
	
	deprClass := Object newAnonymousSubclass. 
	Smalltalk globals at: self globalName put: deprClass new.
	
	deprClass compile: self deprecatedMethodName, ' self deprecated: ''this method is deprecated'''.
	deprClass compile: self nonDeprecatedMethodName.
	
	testClass := Object newAnonymousSubclass.
	