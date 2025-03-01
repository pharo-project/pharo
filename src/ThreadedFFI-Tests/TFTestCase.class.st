Class {
	#name : 'TFTestCase',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'runner',
		'useBytecode'
	],
	#category : 'ThreadedFFI-Tests',
	#package : 'ThreadedFFI-Tests'
}

{ #category : 'building suites' }
TFTestCase class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #runner -> [ TFSameThreadRunner uniqueInstance ]. #useBytecode -> true};
		addCase: { #runner -> [ TFSameThreadRunner uniqueInstance ]. #useBytecode -> false};
		addCase: { #runner -> [ TFWorker named: 'fortest' ]. #useBytecode -> false };
		yourself


]

{ #category : 'executing' }
TFTestCase >> invokeFunction: fun withArguments: arguments [

	^ useBytecode 
		ifTrue: [ self invokeFunctionUsingBytecode: fun withArguments: arguments ]
		ifFalse: [ runner invokeFunction: fun withArguments: arguments ]
	
	
]

{ #category : 'executing' }
TFTestCase >> invokeFunctionUsingBytecode: fun withArguments: arguments [

	| argumentNames builder method |

	runner class = TFSameThreadRunner ifFalse: [ ^ self skip ].

	argumentNames := arguments collectWithIndex: [ :e :i | #_arg_ , i printString ].

	fun validate.

	builder := OCIRBuilder new
		numArgs: arguments size;
		addTemps: argumentNames.

	argumentNames do: [ :aName | builder pushTemp: aName ].
	builder sameThreadCallout: fun.
	
	fun definition returnType isVoid 
		ifTrue: [ builder pushReceiver; returnTop ]
		ifFalse: [ builder returnTop ].
		
	method := builder ir compiledMethod.
	^ method valueWithReceiver: nil arguments: arguments.
]

{ #category : 'accessing' }
TFTestCase >> libraryPath [

	Smalltalk os isUnix ifTrue: [ ^ 'libTestLibrary.so' ].
	Smalltalk os isMacOS ifTrue: [ ^ 'libTestLibrary.dylib' ].
	Smalltalk os isWindows ifTrue: [ ^ 'TestLibrary.dll' ].

	self error: 'Unsupported Platform'
]

{ #category : 'running' }
TFTestCase >> runCaseManaged [

	"Skipping in running on an old VM"
	TFFIBackend isAvailable ifFalse: [ ^ self skip ].

	super runCaseManaged
]

{ #category : 'accessing' }
TFTestCase >> runner [
	^ runner
]

{ #category : 'accessing' }
TFTestCase >> runner: anObject [
	runner := anObject
]

{ #category : 'tests' }
TFTestCase >> shortCallout [
	| fun |
	fun := TFExternalFunction
		name: 'shortCallout'
		moduleName: self libraryPath
		definition:
			(TFFunctionDefinition
				parameterTypes: {}
				returnType: TFBasicType sint32).

	^ self invokeFunction: fun withArguments: #().
]

{ #category : 'accessing' }
TFTestCase >> useBytecode [

	^ useBytecode
]

{ #category : 'accessing' }
TFTestCase >> useBytecode: anObject [

	useBytecode := anObject
]
