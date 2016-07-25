private
keysFor: aBrowserEnvironment 
	| allKeys |
	allKeys := IdentitySet withAll: aBrowserEnvironment keys.
	allKeys addAll: aBrowserEnvironment not keys.
	allKeys removeAll: Smalltalk keys.
	self assert: allKeys isEmpty