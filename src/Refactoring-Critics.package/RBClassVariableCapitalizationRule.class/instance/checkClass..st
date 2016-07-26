running
checkClass: aClass 
	aClass isMeta
		ifTrue: [ ^ self ].
	aClass classVarNames do: [ :each | 
		each first isUppercase ifFalse: [
			result 
				addClass: aClass
				classVariable: each ] ].
	aClass sharedPoolNames do: [ :each | 
		each first isUppercase ifFalse: [ 
			result 
				addClass: aClass
				classVariable: each ] ]