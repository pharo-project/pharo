tests-strings
testWideStringGlobal
	| global globalValue |
	global := 'Foox' asWideString
		at: 4 put: (Character value: 265);
		yourself.
	globalValue := global, 'value'.
	[
		Smalltalk at: global put: globalValue.
		self analyzer considerGlobal: global.

		self
			shouldnt: [ self serialize: 'bar' -> globalValue ]
			raise: Error.
		self assert: self materialized class equals: Association.
		self assert: self materialized value == global
	] ensure: [ Smalltalk removeKey: global ]