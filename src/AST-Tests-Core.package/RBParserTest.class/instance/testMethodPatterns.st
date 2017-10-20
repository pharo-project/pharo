tests
testMethodPatterns
	#(#('+ a ^self + a' #+) #('foo ^self foo' #foo) #('foo: a bar: b ^a + b' #foo:bar:)) 
		do: [:each | self assert: (self parserClass  parseMethodPattern: each first) == each last]