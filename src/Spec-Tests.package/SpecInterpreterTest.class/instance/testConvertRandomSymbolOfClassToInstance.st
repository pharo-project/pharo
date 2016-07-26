tests
testConvertRandomSymbolOfClassToInstance
	
	| symbol instance |
	symbol := #PluggableListMorph.
	
	instance := specInterpreter convertSymbolOfClassToInstance: symbol.
	self assert: (instance class name = symbol)