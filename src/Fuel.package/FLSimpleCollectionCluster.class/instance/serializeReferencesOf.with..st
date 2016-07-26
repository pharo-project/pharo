serialize/materialize
serializeReferencesOf: anObject with: anEncoder
	| refs |
	refs := self references at: anObject ifAbsent: [ ^ self ].
	anEncoder encodePositiveInteger: refs first.
	refs allButFirst do: [ :each |
		anEncoder encodeReferenceTo: each ]
	