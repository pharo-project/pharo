accessing
specialKeys
	^specialKeys
		ifNil: [ 
			specialKeys := self newSpecialKeys ]