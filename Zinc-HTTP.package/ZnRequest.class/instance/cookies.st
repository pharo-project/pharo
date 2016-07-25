accessing
cookies
	| value cookies |
	value := self headers at: 'Cookie' ifAbsent: [ ^ #() ].
	cookies := value isString
		ifTrue: [ value findTokens: #($;) ]
		ifFalse: [ value ].
	^ cookies collect: [ :each | ZnCookie fromString: each ]