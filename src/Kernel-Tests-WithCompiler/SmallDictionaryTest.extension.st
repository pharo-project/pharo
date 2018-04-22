Extension { #name : #SmallDictionaryTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
SmallDictionaryTest >> testStoreOnRoundTrip [
	| dictionary storeString evalutated |
	dictionary := self classToBeTested new.
	dictionary
		add: #a -> 1;
		add: #b -> 30;
		add: #c -> 1;
		add: #d -> -2.
	storeString := String streamContents: [ :s | dictionary storeOn: s ].
	evalutated := Smalltalk compiler evaluate: storeString.
	self assert: dictionary equals: evalutated
]
