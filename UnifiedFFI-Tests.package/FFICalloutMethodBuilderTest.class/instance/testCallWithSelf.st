tests
testCallWithSelf
	| result |
	
	result := self builder build: [ :builder |	
		builder requestor requestor: FFICalloutObjectForTest.
		builder 
			signature: #( void method1 ( self, int *var2 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: FFICalloutObjectForTest;
				yourself )].

	self assert: result notNil.
	self assert: result isCompiledMethod.
	self assert: result primitive = 0.
	self assert: result literals first name = 'method1'.
	self assert: result literals first argTypes last = ExternalType long asPointerType. 