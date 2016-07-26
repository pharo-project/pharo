tests
testStoreString
	self assert: $a storeString = '$a'.
	self assert: $5 storeString = '$5'.
	self assert: $@ storeString = '$@'.

	self assert: Character cr storeString equals: 'Character cr'.
	self assert: Character lf storeString equals: 'Character lf'.
	self assert: Character space storeString equals: 'Character space'.
	self assert: (Character value: 0) storeString equals: 'Character null'.
	
	self assert: (Character value: 17) storeString equals: '(Character value: 17)'.