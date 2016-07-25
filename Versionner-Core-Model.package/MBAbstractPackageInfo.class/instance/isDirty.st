testing
isDirty
	| wc |
	wc := self workingCopy.
	wc ifNil: [ ^false ].
	^wc needsSaving