tests
testMethodPatterns
	#(#('+ a ^self + a' #+) #('foo ^self foo' #foo) #('foo: a bar: b ^a + b' #foo:bar:)) 
		do: [:each | self assert: (RBParser parseMethodPattern: each first) == each last]