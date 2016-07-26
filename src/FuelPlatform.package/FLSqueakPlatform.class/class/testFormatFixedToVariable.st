private-extensions-flmigrationtest
testFormatFixedToVariable
	"Tests that serializer can tolerate a format change from a *fixed* class to *weak/variable*.
	Notice, however, that the other way around (variable to fixed) doesn't work so far"

	<flExtensionOf: #FLMigrationTest>
	| materialized |
	self serialize:
		(Smalltalk globals at: #FLPair) new.
	materialized := [ self
		during: [ self materialized ]
		rename: (Smalltalk globals at: #FLPair)
		as: (Smalltalk globals at: #FLWeakClassMock) ]
		on: (Smalltalk globals at: #AttemptToWriteReadOnlyGlobal)
		do: [ :ex | ex resume: true ].
	self assert: materialized size = 0