asserting
assert: aSelector orders: sexpr as: expected unresolved: unresolved
	| missing |
	missing := OrderedCollection new.
	version := self versionFromTree: sexpr.
	version 
		perform: aSelector 
		with: [:ea | visited add: ea info name]
		with: [:ea | missing add: ea name].
	self assert: visited asArray = expected.
	self assert: missing asArray = unresolved.