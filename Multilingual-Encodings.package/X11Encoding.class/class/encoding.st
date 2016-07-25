accessing
encoding

	| enc |
	enc := self getEncoding.
	enc ifNil: [ ^ nil ].
	^ enc asLowercase.