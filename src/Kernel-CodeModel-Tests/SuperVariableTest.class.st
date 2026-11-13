Class {
	#name : 'SuperVariableTest',
	#superclass : 'TestCase',
	#category : 'Kernel-CodeModel-Tests-Variables',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Variables'
}

{ #category : 'tests' }
SuperVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #super.
	self assert: (var readInContext: thisContext) identicalTo: super.
	"read from a block context"
	self assert: [(var readInContext: thisContext)] value identicalTo: super
]

{ #category : 'tests' }
SuperVariableTest >> testReadInContextClean [
	<compilerOptions: #( +optionCleanBlockClosure)>
	| block |

	block := [ (thisContext lookupVar: #super) ].
	self assert: block value equals: nil
]

{ #category : 'tests' }
SuperVariableTest >> testUsingMethods [

	|  var |
	var := super class lookupVar: #super.
	self assert: (var usingMethods includes: thisContext method)
]
