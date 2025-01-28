Class {
	#name : 'BenchTFFISameThreadUsingBytecode',
	#superclass : 'BenchTFFI',
	#category : 'ThreadedFFI-UFFI-Tests-Benchs',
	#package : 'ThreadedFFI-UFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'private' }
BenchTFFISameThreadUsingBytecode >> doCallWith: aFloat another: aDouble [

	| definition function |

	<opalBytecodeMethod>
	
	definition := TFFunctionDefinition
		        parameterTypes: {
				        TFBasicType float.
				        TFBasicType double }
		        returnType: TFBasicType float.

	function := TFExternalFunction
		        name: #sumAFloatAndADouble  
				  moduleName: TFTestLibraryUsingSameThreadRunner uniqueInstance libraryName
		        definition: definition.
		
	^ OCIRBuilder buildIR: [ :builder |
		  builder
			  numArgs: 2;
			  addTemps: #( #aFloat #aDouble );
			  pushTemp: #aFloat;
			  send: #asFloat;
			  pushTemp: #aDouble;
			  send: #asFloat;
			  sameThreadCallout: function;
			  returnTop ]
]

{ #category : 'private' }
BenchTFFISameThreadUsingBytecode >> doCallWithByteArray: aByteArray [

	| definition function |

	<opalBytecodeMethod>
	
	definition := TFFunctionDefinition
		        parameterTypes: {
				        TFBasicType pointer }
		        returnType: TFBasicType uint64.

	function := TFExternalFunction
		        name: #id_int  
				  moduleName: TFTestLibraryUsingSameThreadRunner uniqueInstance libraryName
		        definition: definition.
		
	^ OCIRBuilder buildIR: [ :builder |
		  builder
			  numArgs: 1;
			  addTemps: #( #aByteArray );
			  pushTemp: #aByteArray;
"			  send: #tfPointerAddress;
"			  sameThreadCallout: function;
			  returnTop ]
]

{ #category : 'private' }
BenchTFFISameThreadUsingBytecode >> doCallWithExternalAddress: aExternalAddress [

	| definition function |

	<opalBytecodeMethod>
	
	definition := TFFunctionDefinition
		        parameterTypes: {
				        TFBasicType pointer }
		        returnType: TFBasicType uint64.

	function := TFExternalFunction
		        name: #id_int  
				  moduleName: TFTestLibraryUsingSameThreadRunner uniqueInstance libraryName
		        definition: definition.
		
	^ OCIRBuilder buildIR: [ :builder |
		  builder
			  numArgs: 1;
			  addTemps: #( #aExternalAddress );
			  pushTemp: #aExternalAddress;
			  send: #tfPointerAddress;
			  sameThreadCallout: function;
			  returnTop ]
]

{ #category : 'private' }
BenchTFFISameThreadUsingBytecode >> doPointerVoid: aPointer [

	| definition function |

	<opalBytecodeMethod>
	
	definition := TFFunctionDefinition
		        parameterTypes: { TFBasicType pointer }
		        returnType: TFBasicType void.

	function := TFExternalFunction
		        name: #id_int  
				  moduleName: TFTestLibraryUsingSameThreadRunner uniqueInstance libraryName
		        definition: definition.
		
	^ OCIRBuilder buildIR: [ :builder |
		  builder
			  numArgs: 1;
			  addTemps: #( #aPointer );
			  pushTemp: #aPointer;
"			  send: #tfPointerAddress;
"			  sameThreadCallout: function;
			  returnTop ]
]

{ #category : 'running' }
BenchTFFISameThreadUsingBytecode >> runCall [
	<script: 'self new runCall traceCr'>

	(self class >> #doCallWith:another: literalAt: 2) validate.
	^ super runCall
]

{ #category : 'running' }
BenchTFFISameThreadUsingBytecode >> runCallWithByteArray [
	<script: 'self new runCallWithByteArray traceCr'>

	(self class>> #doCallWithByteArray:) literals select: [:e | e class = TFExternalFunction] thenDo: [:e | e validate].
	^ super runCallWithByteArray
]

{ #category : 'running' }
BenchTFFISameThreadUsingBytecode >> runCallWithExternalAddress [
	<script: 'self new runCallWithExternalAddress traceCr'>

	(self class>> #doCallWithExternalAddress:) literals select: [:e | e class = TFExternalFunction] thenDo: [:e | e validate].
	^ super runCallWithExternalAddress
]

{ #category : 'running' }
BenchTFFISameThreadUsingBytecode >> runCallWithPointerVoid [
	<script: 'self new runCallWithPointerVoid traceCr'>

	(self class>> #doPointerVoid: ) literals select: [:e | e class = TFExternalFunction] thenDo: [:e | e validate].
	^ super runCallWithPointerVoid
]

{ #category : 'running' }
BenchTFFISameThreadUsingBytecode >> runCallWithPointerVoidWithByteArray [
	<script: 'self new runCallWithPointerVoidWithByteArray traceCr'>

	(self class>> #doPointerVoid: ) literals select: [:e | e class = TFExternalFunction] thenDo: [:e | e validate].
	^ super runCallWithPointerVoidWithByteArray
]
