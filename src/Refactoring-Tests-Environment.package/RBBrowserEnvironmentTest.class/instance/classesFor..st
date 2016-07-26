private
classesFor: aBrowserEnvironment 
	| allClasses |
	allClasses := aBrowserEnvironment classes asSet.
	allClasses addAll: aBrowserEnvironment not classes.
	RBBrowserEnvironment new allClassesDo: [:each | allClasses remove: each].
	self assert: allClasses isEmpty