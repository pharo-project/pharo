Class {
	#name : 'OCSpecialSelectorTest',
	#superclass : 'OCASTTranslatorTest',
	#instVars : [
		'optimisationsActive',
		'replacedMethods'
	],
	#category : 'OpalCompiler-Tests-AST',
	#package : 'OpalCompiler-Tests',
	#tag : 'AST'
}

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> compilationContext [

	| context |
	context := super compilationContext.

	context parseOptions: {
			(optimisationsActive
				 ifTrue: [ #+ ]
				 ifFalse: [ #- ]).
			#optionOptimiseSpecialSends }.
	^ context
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> proxy: aMethod [
	"Replace the method by the proxy and keep the original one to restore it on tearDown"

	| proxy |
	proxy := OCCalledMethodProxy on: aMethod.
	self replace: aMethod with: proxy.
	^ proxy
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> replace: aMethod with: aReplacement [

	"Replace the method in its class and remember it to restore it at the end."
	replacedMethods add: aMethod.
	
	"Make sure we install the method after remembering it to avoid calling it when adding it in the collection"
	aMethod methodClass
		addSelectorSilently: aMethod selector
		withMethod: aReplacement.
]

{ #category : 'running' }
OCSpecialSelectorTest >> setUp [

	super setUp.
	optimisationsActive := true.
	replacedMethods := Set new
]

{ #category : 'running' }
OCSpecialSelectorTest >> tearDown [
	"Restore methods replaced during the test"

	replacedMethods do: [ :e |
		e methodClass addSelectorSilently: e selector withMethod: e ].
	super tearDown
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> testOptimisedPlusSpecialSendsMessageDoesNotCaptureSend [
	"Turn on optimisation, then compile & run"

	| proxy |
	optimisationsActive := true.
	self compileExample: #exampleReturn1plus2.

	[
		proxy := self proxy: SmallInteger >> #+.

		self deny: proxy wasCalled.
		self assert: (self executeMethod: method onReceiver: instance) equals: 3.
		self deny: proxy wasCalled.
	] valueUnpreemptively
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> testOptimisedValueSpecialSendsMessageDoesNotCaptureSend [
	"Turn on optimisation, then compile & run"

	| proxy |
	optimisationsActive := true.
	self compileExample: #exampleSimpleBlockiVar.
	instance iVar: #valueToTest.
	[
		self replace: OCOpalExamples >> #exampleSimpleBlockiVar with: method.
		proxy := self proxy: BlockClosure >> #value.

		self deny: proxy wasCalled.
		"Do not directly execute the method here.
		Executing a method through valueWithReceiver: forces JIT compilation, which does not optimise #value sends.
		Instead, send a message to go through the interpreter and have an optimised execution that does not invoke the proxy"
		self assert: instance exampleSimpleBlockiVar equals: #valueToTest.
		self deny: proxy wasCalled.
	] valueUnpreemptively
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> testUnoptimisedPlusSpecialSendsMessageCapturesSend [
	"Turn off optimisation, then compile & run"

	| proxy |
	optimisationsActive := false.
	self compileExample: #exampleReturn1plus2.
	[
	proxy := self proxy: SmallInteger >> #+.

	self deny: proxy wasCalled.
	self
		assert: (self executeMethod: method onReceiver: instance)
		equals: 3.
	self assert: proxy wasCalled ] valueUnpreemptively
]

{ #category : 'tests - simple' }
OCSpecialSelectorTest >> testUnoptimisedValueSpecialSendsMessageCapturesSend [
	"Turn off optimisation, then compile & run"

	| proxy |
	optimisationsActive := false.
	self compileExample: #exampleSimpleBlockiVar.
	instance iVar: #valueToTest.
	[
	proxy := self proxy: BlockClosure >> #value.

	self deny: proxy wasCalled.
	self
		assert: (self executeMethod: method onReceiver: instance)
		equals: #valueToTest.
	self assert: proxy wasCalled ] valueUnpreemptively
]
