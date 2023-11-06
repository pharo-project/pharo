Class {
	#name : 'OCDoitTest',
	#superclass : 'TestCase',
	#instVars : [
		'ivarForTesting'
	],
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'binding' }
OCDoitTest >> bindingOf: aSelector [
	^(aSelector == #requestorVarForTesting)
		ifTrue: [WorkspaceVariable key: aSelector value: 5 ]
		ifFalse:  [nil]
]

{ #category : 'binding' }
OCDoitTest >> canAddBindingOf: aSymbol [
	^ true
]

{ #category : 'binding' }
OCDoitTest >> hasBindingOf: aSelector [
	^aSelector == #requestorVarForTesting
]

{ #category : 'binding' }
OCDoitTest >> needRequestorScope [
	^true
]

{ #category : 'interactive error protocol' }
OCDoitTest >> notify: aString at: anInteger in: aString3 [
	"we ignore"
]

{ #category : 'tests' }
OCDoitTest >> testDoItContextReadGlobal [
	"we can read a global from a doit when executing against a context"

	|  value |
	value := thisContext compiler evaluate: 'Object'.
	self assert: value equals: Object
]

{ #category : 'tests' }
OCDoitTest >> testDoItContextReadIvar [

	| value method |
	"we can read this ivar from a doit when executing against thisContext"
	ivarForTesting := #someValue.

	method := thisContext compiler compile: 'ivarForTesting'.
	value := method valueWithReceiver: self arguments: #().
	self assert: value equals: #someValue
]

{ #category : 'tests' }
OCDoitTest >> testDoItContextReadIvarSubclass [

	| ctx value method |
	"Get a method compiled for a base class"
	method := Object compiler compile: 'foo ^thisContext'.
	ctx := self executeMethod: method.

	ivarForTesting := #someValue.
	value := ctx compiler evaluate: 'ivarForTesting'.
	self assert: value equals: #someValue.

	value := ctx compiler evaluate: 'ivarForTesting := #someOtherValue'.
	self assert: ivarForTesting equals: #someOtherValue
]

{ #category : 'tests' }
OCDoitTest >> testDoItContextReadTemp [
	| tempForTesting value |
	"we can read this temp from a doit when executing against thisContext"
	tempForTesting := #someValue.

	value := thisContext compiler evaluate: 'tempForTesting'.

	self assert: value equals: #someValue
]

{ #category : 'tests' }
OCDoitTest >> testDoItContextReadThisContext [

	| value |
	"We can read read thisContext in a DoIt and it is the context that we evaluate against, not the context of the doit.
	 We shadow the ThisContextVarable using a DoItVariable created by PseudoVariable>>#asDoItVariableFrom:"

	value := thisContext compiler evaluate: 'thisContext'.
	
	"if we would not shadow, we would read the context of the DoIt"
	self deny: value selector identicalTo: #DoIt.
	"but instead we read the context that we evaluated against"
	self assert: value identicalTo: thisContext.
	self assert: value selector identicalTo: #testDoItContextReadThisContext
]

{ #category : 'tests' }
OCDoitTest >> testDoItExtraBindings [
	"We can use extra bindings in a DoIt"

	| result |
	result := OpalCompiler new
		bindings: {  #binding -> 1};
		evaluate: 'binding'.
	self assert: result equals: 1
]

{ #category : 'tests' }
OCDoitTest >> testDoItHalt [

	self should: [OpalCompiler new
			evaluate: 'self halt'] raise: Halt
]

{ #category : 'tests' }
OCDoitTest >> testDoItHaltBinding [
	"We can evaluate with a halt and extra bindings"

	self should: [OpalCompiler new
			bindings: {#binding -> 1 };
			evaluate: 'self halt'] raise: Halt
]

{ #category : 'tests' }
OCDoitTest >> testDoItRequestorEvalError [
	| value |
	value  := OpalCompiler new
	requestor: self;
	evaluate: '1('.

	self assert: value isNil
]

{ #category : 'tests' }
OCDoitTest >> testDoItRequestorOptimized [

	| value  |
	"Check optimzied to:do:, we call lookupVar: on the synthesized limit var, see canAddBindingOf:"
	value := OpalCompiler new
		          requestor: self;
		          evaluate: '
							| sum |
							sum := 0.
							1 to: requestorVarForTesting do: [:each | sum :=sum+1].
							sum'.

	self assert: value equals: 5
]

{ #category : 'tests' }
OCDoitTest >> testDoItRequestorReadRequestorVar [

	| value  |
	"we can read a variable from a requestor"
	value := OpalCompiler new
		          requestor: self;
		          evaluate: 'requestorVarForTesting'.

	self assert: value equals: 5
]

{ #category : 'tests' }
OCDoitTest >> testDoItRequestorShadow [

	| value  |
	"we can shadow a var defined in the requestor without error"
	value := OpalCompiler new
		          requestor: self;
		          evaluate: '|requestorVarForTesting| requestorVarForTesting'.

	self assert: value equals: nil
]

{ #category : 'tests' }
OCDoitTest >> testDoitContextCheckClass [
	"if we create a parse tree for a doit in a context, the class should be correctly set"

	| ast method |
	method := thisContext compiler compile: 'self'.
	ast := method ast.
	"we expect that the class is set to the class of the context, which is self class"
	self assert: ast methodClass equals: self class
]
