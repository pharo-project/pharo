private
namesIn: aString
	| names scanner token |
	names := OrderedCollection new.
	scanner := RBScanner
		on: (ReadStream on: aString)
		errorBlock: [ :msg :pos | ^ names ].
	[ scanner atEnd ] whileFalse: [
		token := scanner next.
		token isIdentifier
			ifTrue: [ names add: token value ] ].
	^ names asArray