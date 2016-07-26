tests
testThatExistingPackagenamesDoesNotContainIllegalCharacters
	| illegalCharacters | 
	
	illegalCharacters := #(  $\ $/ $: $* $? $" $< $> $|  ).
	
	self class environment organization categories do: [ :aPackageName |
		self deny: (aPackageName includesAnyOf: illegalCharacters)
	]