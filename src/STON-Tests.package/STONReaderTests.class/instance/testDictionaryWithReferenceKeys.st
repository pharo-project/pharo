tests
testDictionaryWithReferenceKeys
	| keysCollection dictionary ston object |
	keysCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	dictionary := Dictionary new.
	keysCollection doWithIndex: [ :each :index | 
		dictionary at: each put: index ].
	object := Array with: keysCollection with: dictionary.
	ston := STON toStringPretty: object.
	object := (STON reader on: ston readStream) next.
	object first doWithIndex: [ :each :index |
		self assert: (object second at: each) equals: index ].
	self assert: object second isHealthy 