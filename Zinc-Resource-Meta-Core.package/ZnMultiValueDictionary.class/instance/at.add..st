accessing
at: key add: value
	"Store value under key. If key already exists, store multiple values as Array"
	
	| existingValue newValue |
	existingValue := self at: key ifAbsent: [ nil ].
	newValue := existingValue isNil
		ifTrue: [ value ] 
		ifFalse: [
			existingValue isArray 
				ifTrue: [ existingValue copyWith: value ]
				ifFalse: [ Array with: existingValue with: value ] ].
	^ self at: key put: newValue 

