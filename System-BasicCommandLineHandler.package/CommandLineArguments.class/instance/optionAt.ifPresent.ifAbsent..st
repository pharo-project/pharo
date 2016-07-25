accessing
optionAt: aString ifPresent: presentBlock ifAbsent: absentBlock
	| option |
	option := self optionAt: aString ifAbsent: [ ^ absentBlock value ].
	^ presentBlock value: option