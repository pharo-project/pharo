converting
buildStringOrText
	| string cv specVersion attributes wc |
	attributes := OrderedCollection new.
	[ string := super buildStringOrText ]
		on: Error
		do: [ :ex | 
			attributes add: TextEmphasis struckOut.
			^ Text string: self name asString attributes: attributes ].
	((wc := self workingCopy) notNil and: [ wc needsSaving ])
		ifTrue: [ attributes add: TextEmphasis underlined ].
	self currentVersion. "prime the pump"
	currentVersion == #notLoaded
		ifTrue: [ 
			attributes notEmpty
				ifTrue: [ ^ Text string: string attributes: attributes ].
			^ string ].
	super isDirty | self currentVersionMismatch
		ifTrue: [ attributes add: TextEmphasis bold ].
	self currentVersion versionStatus == #somethingLoaded
		ifTrue: [ attributes add: TextColor red ].
	attributes notEmpty
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string