tests
testAllAtomicTypesCall
	| result |
	
	result := self builder build: [ :builder |	
		builder 
			signature: #( void method1 (bool var1, byte var2, char var3, double var4, float var5, long var6, longlong var7, sbyte var8, schar var9, short var10, ulong var11, ulonglong var12, ushort var13) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #methodV1:v2:v3:v4:v5:v6:v7:v8:v9:v10:v11:v12:v13:;
				argumentNames: #('var1' 'var2' 'var3' 'var4' 'var5' 'var6' 'var7' 'var8' 'var9' 'var10' 'var11' 'var12' 'var13');
				methodClass: Object;
				yourself )].
	
	self assert: result notNil.
	self assert: result isCompiledMethod.
	self assert: result primitive = 0.
	self assert: result literals first name = 'method1'.
	self assert: result literals first argTypes = { 
		ExternalType void. 
		ExternalType bool. 
		ExternalType byte. 
		ExternalType char. 
		ExternalType double. 
		ExternalType float.
		architecture externalLongType.
		ExternalType signedLongLong. 
		ExternalType sbyte.
		"schar and sbyte are the same size, and NB retrieves sbyte instead :("
		ExternalType sbyte. 
		ExternalType short. 
		architecture externalULongType. 
		ExternalType unsignedLongLong. 
		ExternalType ushort }