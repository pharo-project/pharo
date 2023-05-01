Class {
	#name : #OCPragmaTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Source'
}

{ #category : #'method - tested' }
OCPragmaTest >> methodDoublePragma [
	^'methodDoublePragma
		<hello: 5>
		<hello: 2>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodNoPragma [
	^'methodNoPragma: aNum
		^aNum'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPragmaAfterBeforTemps [
	^'methodPragmaAfterBeforTemps
		<hello: 5>
		| aTemp |
		<world: #ok>
		<blue: true>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPragmaTwoParam [
	^'methodDoublePragma
		<hello: 5 by: 2>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPragmaUnarayMessage [
	^'methodPragmaUnarayMessage
		<hello>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPrimitive [
	^'methodPrimitive
		<primitive: 4>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPrimitivePragma [
	^'methodPrimitivePragma
		<hello: 1>
		<primitive: 4>
		<hello: 1>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPrimitiveString [
	^'methodPrimitiveString
		<primitive: ''aFunctionName''>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodPrimitiveStringModule [
	^'methodPrimitiveStringModule
		<primitive: ''aFunctionName'' module: ''aModuleName''>'
]

{ #category : #'method - tested' }
OCPragmaTest >> methodSinglePragma [
	^'methodSinglePragma
		<hello: 5>'
]

{ #category : #tests }
OCPragmaTest >> testDoublePragma [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodDoublePragma.

	self assert: aCompiledMethod pragmas first selector equals: #hello:.
	self assert: aCompiledMethod pragmas second selector equals: #hello:
]

{ #category : #tests }
OCPragmaTest >> testIsPrimitive [
	| aRBMethod |

	aRBMethod := OpalCompiler new parse: self methodPrimitive.
	self assert: aRBMethod isPrimitive
]

{ #category : #tests }
OCPragmaTest >> testNoPragma [

	OpalCompiler new parse: self methodNoPragma
]

{ #category : #tests }
OCPragmaTest >> testPragmaAfterBeforTemp [
	| aComiledMethod |
	aComiledMethod := OpalCompiler new compile: self methodPragmaAfterBeforTemps.

	self assert: aComiledMethod pragmas first selector equals: #hello:.
	self assert: aComiledMethod pragmas second selector equals: #world:
]

{ #category : #tests }
OCPragmaTest >> testPragmaTwoParam [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPragmaTwoParam.

	self assert: aCompiledMethod pragmas first selector equals: #hello:by:
]

{ #category : #tests }
OCPragmaTest >> testPragmaUnarayMessage [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPragmaUnarayMessage.

	self assert: aCompiledMethod pragmas first selector equals: #hello
]

{ #category : #tests }
OCPragmaTest >> testPrimitiveNumber [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPrimitive.
	self assert: aCompiledMethod primitive equals: 4
]

{ #category : #tests }
OCPragmaTest >> testPrimitivePragmaNumber [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPrimitivePragma.
	self assert: aCompiledMethod primitive equals: 4
]

{ #category : #tests }
OCPragmaTest >> testPrimitiveString [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPrimitiveString.

	self assert: aCompiledMethod primitive equals: 117
]

{ #category : #tests }
OCPragmaTest >> testPrimitiveStringModule [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodPrimitiveStringModule.

	self assert: aCompiledMethod primitive equals: 117
]

{ #category : #tests }
OCPragmaTest >> testSinglePragma [
	| aCompiledMethod |
	aCompiledMethod := OpalCompiler new compile: self methodSinglePragma.
	self assert: aCompiledMethod pragmas first selector equals: #hello:
]
