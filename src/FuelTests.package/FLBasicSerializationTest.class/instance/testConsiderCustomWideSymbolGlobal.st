tests-globals
testConsiderCustomWideSymbolGlobal
	"The same than #testConsiderCustomGlobal but with a WideSymbol."
	
	| aWideSymbol aPerson |
	
	aWideSymbol := (WideString streamContents: [ :stream |
		256 to: 260 do: [ :code | 
			stream nextPut: code asCharacter ] ]) asSymbol.

	self analyzer considerGlobal: aWideSymbol.

	aPerson := FLPerson new.
	
	[ 
		Smalltalk globals at: aWideSymbol put: aPerson.
		self
			shouldnt: [ self assertSerializationIdentityOf: aPerson ]
			raise: Error
	] ensure: [ Smalltalk globals removeKey: aWideSymbol ].
