converting
buildStringOrText
	| string cv projectClass info attributes specVersion |
	string := super buildStringOrText.
	attributes := OrderedCollection new.
	self isDirty
		ifTrue: [ attributes add: TextEmphasis underlined ].
	(((projectClass := self spec projectClass) isNil or: [ (cv := self currentVersion) isEmpty ])
		or: [ (specVersion := self spec versionOrNil) isNil ])
		ifTrue: [ 
			attributes notEmpty
				ifTrue: [ ^ Text string: string attributes: attributes ].
			^ string ].
	self currentVersionMismatch
		ifTrue: [ attributes add: TextEmphasis bold ].
	specVersion versionStatus == #somethingLoaded
		ifTrue: [ attributes add: TextColor red ].
	attributes notEmpty
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string