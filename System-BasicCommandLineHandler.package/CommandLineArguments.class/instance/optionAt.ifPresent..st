accessing
optionAt: aString ifPresent: presentBlock
	| option |
	
	option := self optionAt: aString ifAbsent: [ ^ self ].
	^ presentBlock value: option