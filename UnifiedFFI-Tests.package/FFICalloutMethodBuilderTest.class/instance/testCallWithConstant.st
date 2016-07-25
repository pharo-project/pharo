tests
testCallWithConstant
	| result |
	
	result := self builder build: [ :builder |	
		builder 
			signature: #( void method1 ( int var1, 42 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: Object;
				yourself )].

	self assert: result notNil.
	self assert: result isCompiledMethod.
	self assert: result primitive = 0.
	self assert: result literals first name = 'method1'.