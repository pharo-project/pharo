instance creation
new
	| registry |
	registry := super new.
	WeakArray addWeakDependent: registry.
	^registry
