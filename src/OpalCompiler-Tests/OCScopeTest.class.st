Class {
	#name : 'OCScopeTest',
	#superclass : 'TestCase',
	#instVars : [
		'ivarForTesting'
	],
	#classVars : [
		'MyClassVarForTesting'
	],
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'accessing' }
OCScopeTest class >> myClassVarForTesting [

	^MyClassVarForTesting
]

{ #category : 'accessing' }
OCScopeTest >> ivarForTesting [

	^ ivarForTesting
]
