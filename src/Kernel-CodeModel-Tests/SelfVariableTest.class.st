Class {
	#name : 'SelfVariableTest',
	#superclass : 'TestCase',
	#category : 'Kernel-CodeModel-Tests-Variables',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Variables'
}

{ #category : 'tests' }
SelfVariableTest >> testReadInContext [

	| var |
	var := self class lookupVar: #self.
	self assert: (var readInContext: thisContext) identicalTo: self.
	"read from a block context"
	self assert: [(var readInContext: thisContext)] value identicalTo: self
]

{ #category : 'tests' }
SelfVariableTest >> testReadInContextClean [
	<compilerOptions: #( +optionCleanBlockClosure)>
	| block |

	block := [ (thisContext lookupVar: #self) ].
	self assert: block value equals: nil
]

{ #category : 'tests' }
SelfVariableTest >> testUsingMethods [

	|  var |
	var := self class lookupVar: #self.
	self assert: (var usingMethods includes: thisContext method)
]

{ #category : 'tests' }
SelfVariableTest >> testUsingMethodsSuper [

	|  var |
	"we use super instead of self to check that super sends are not understood as self sends"
	var := super class lookupVar: #self.
	super deny: (var usingMethods includes: thisContext method)
]
