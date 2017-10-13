tests
testConcatenationIsSymbol
	"Concatenating 2 symbols results in another symbol"
	self assert: (#foo , #bar) isSymbol.
	self assert: (#foo , #bar) == #foobar.
	
	"Concatenating the empty Symbol does not change a Symbol"
	self assert: (#foo , emptySymbol) == #foo.
	self assert: (emptySymbol , #foo) == #foo.
	
	"Strings and Symbols can still be mixed, the receiver determines the result type"
	"Symbol receiver gives Symbol result"
	self assert: (#foo , 'bar') isSymbol.
	self assert: (#foo , 'bar') == #foobar.
	"String receiver gives String result"
	self assert: ('foo' , #bar) isString.
	self assert: ('foo' , #bar) = 'foobar'.
	"Strings and Symbols still compare content-wise"
	self assert: ('foo' , #bar) = #foobar.
	"But Strings and Symbols are not identical"
	self deny: ('foo' , #bar) == #foobar.
