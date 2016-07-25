asserting
assert: aSelector orders: sexpr as: array
	| expected |
	expected := OrderedCollection new.
	version := self versionFromTree: sexpr.
	version perform: aSelector with: [:ea | expected add: ea info name].
	self assert: expected asArray = array