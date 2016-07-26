accessing
cookies
	| value cookies |
	value := self headers at: 'Set-Cookie' ifAbsent: [ ^ #() ].
	cookies := value isString
		ifTrue: [ Array with: value ]
		ifFalse: [ value ].
	^ cookies collect: [ :each | ZnCookie fromString: each ]