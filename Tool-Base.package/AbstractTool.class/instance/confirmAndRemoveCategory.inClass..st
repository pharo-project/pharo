private
confirmAndRemoveCategory: aCategory inClass: aClass
	| question answer |
	question := 'Are you sure you want to\remove the protocol {1}\and all its methods?'.
	answer := self confirm: (question withCRs format: {aCategory}).
	answer ifTrue: [ aClass removeProtocol: aCategory ].
	^ answer