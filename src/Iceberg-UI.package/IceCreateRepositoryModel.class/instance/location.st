accessing
location
	| location |
	location := localDirectoryLocation location.
	^ (location notNil and: [ location pathString ~= self defaultLocation ])
		ifTrue: [ ^ location ]
		ifFalse: [ nil ]