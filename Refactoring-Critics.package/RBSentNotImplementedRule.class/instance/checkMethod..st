running
checkMethod: aMethod
	| messages |
	messages := aMethod messages reject: [ :each | each isSelectorSymbol].
	messages do: [ :m | result
					   addSearchString: m;
						addMethod: aMethod ]