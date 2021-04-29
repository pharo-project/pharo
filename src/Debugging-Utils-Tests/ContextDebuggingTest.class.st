Class {
	#name : #ContextDebuggingTest,
	#superclass : #TestCase,
	#instVars : [
		'instVar'
	],
	#category : #'Debugging-Utils-Tests'
}

{ #category : #helpers }
ContextDebuggingTest >> accessorMethod [
	^instVar
]

{ #category : #helpers }
ContextDebuggingTest >> contextForMethod: aCompiledMethod [

	^ Context
		  sender: thisContext
		  receiver: self class basicNew
		  method: aCompiledMethod
		  arguments: #(  )
]

{ #category : #helpers }
ContextDebuggingTest >> deadContext [
	| process context |
	process := [  ] newProcess.
	context := process suspendedContext.
	context pc: nil.
	^ context
]

{ #category : #helpers }
ContextDebuggingTest >> selfReturnMethod [
	#foo asSymbol 
]

{ #category : #tests }
ContextDebuggingTest >> testStepToSendOrReturn [
	| context |
	context := self deadContext.
	self assert: context isDead.
	context := context stepToSendOrReturn.
	self assert: context isDead
]

{ #category : #tests }
ContextDebuggingTest >> testUpdatePCForQuickPrimitiveRestart [

	|method newContext  |
	method := self class >> #accessorMethod.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method initialPC + newContext quickMethodPrimitiveBytecodeSize.
		
	method := self class >> #zeroReturnMethod.
	newContext := self contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method initialPC + newContext quickMethodPrimitiveBytecodeSize.
		
	method := self class >> #trueReturnMethod.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method endPC.
		
	method := Symbol >> #asSymbol.
	newContext := self  contextForMethod: method.
	newContext updatePCForQuickPrimitiveRestart.	
	self
		assert: newContext pc
		equals: method endPC
]

{ #category : #helpers }
ContextDebuggingTest >> trueReturnMethod [
	^true
]

{ #category : #helpers }
ContextDebuggingTest >> zeroReturnMethod [
	^0
]
