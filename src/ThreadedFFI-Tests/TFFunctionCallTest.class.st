Class {
	#name : #TFFunctionCallTest,
	#superclass : #TFTestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #tests }
TFFunctionCallTest >> testCallbackAsFunction [

	| callback fun definition returnValue |

	callback := TFCallback 
		forCallback: [ :a :b | a + b ] 
		parameters: { TFBasicType sint. TFBasicType float } 
		returnType: TFBasicType float
		runner: runner.

	definition := TFFunctionDefinition
		parameterTypes: {TFBasicType sint. TFBasicType float.}
		returnType: TFBasicType float.

	fun := TFExternalFunction 
		fromAddress: callback getHandle 
		definition: definition.
	
	returnValue := runner invokeFunction: fun withArguments: {1. 2.0}.
			
	self assert: returnValue equals: 3.0
]

{ #category : #tests }
TFFunctionCallTest >> testCallbackInLoop [

	| callback fun returnValue |

	callback := TFCallback 
		forCallback: [ :a | a + 1 ] 
		parameters: { TFBasicType sint. } 
		returnType: TFBasicType sint
		runner: runner.

	fun := TFExternalFunction
		name: 'callbackInALoop'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {TFBasicType pointer}
			returnType: TFBasicType sint).

	returnValue := runner invokeFunction: fun withArguments: {callback getHandle}.

	self assert: returnValue equals: 42

]

{ #category : #tests }
TFFunctionCallTest >> testCallbackInSingleFunction [

	| callback fun returnValue |

	callback := TFCallback 
		forCallback: [ :a | a + 1 ] 
		parameters: { TFBasicType sint. } 
		returnType: TFBasicType sint
		runner: runner.

	fun := TFExternalFunction
		name: 'singleCallToCallback'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {TFBasicType pointer. TFBasicType sint}
			returnType: TFBasicType sint).
			
	returnValue := runner invokeFunction: fun withArguments: {callback getHandle. 3}.

	self assert: returnValue equals: 5

]

{ #category : #tests }
TFFunctionCallTest >> testWithFloatAndDouble [
	| fun return |

	fun := TFExternalFunction
		name: 'sumAFloatAndADouble'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: { TFBasicType float. TFBasicType double }
			returnType: TFBasicType float).
			
	return := runner invokeFunction: fun withArguments: #(1.0 2.5).
		
	self assert: return equals: 3.5
]

{ #category : #tests }
TFFunctionCallTest >> testWithTwoInts [

	| fun return |

	fun := TFExternalFunction
		name: 'sumTwoNumbers'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: { TFBasicType sint. TFBasicType sint }
			returnType: TFBasicType sint).
				
	return := runner invokeFunction: fun withArguments: {3. 2}.
	
	self assert: return equals: 5
]

{ #category : #tests }
TFFunctionCallTest >> testWithoutArguments [
	
	self assert: self shortCallout equals: 42.
]
