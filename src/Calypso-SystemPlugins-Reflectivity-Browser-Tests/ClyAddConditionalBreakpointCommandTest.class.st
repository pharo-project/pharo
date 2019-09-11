Class {
	#name : #ClyAddConditionalBreakpointCommandTest,
	#superclass : #TestCase,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Tests-Breakpoints'
}

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> parserClass [
	^ RBParser
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testMessageSend_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'val msg: arg')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ((ThisContext lookupSymbol: #val) msg: (ThisContext lookupSymbol: #arg))]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testSelf_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'self')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext receiver ]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testSimpleCondition_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'true')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: true ]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testSuperSendNoArg_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'super msg')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #msg withArguments: {} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testSuperSendWithArgs_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'super foo: arg1 bar: arg2')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #foo:bar: withArguments: {ThisContext lookupSymbol: #arg1. ThisContext lookupSymbol: #arg2.} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testVariableLookup_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'var')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #var)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testVariableNamedThisContext_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'ThisContext')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #ThisContext)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTest >> testthisContext_conditionBlockProducerMethodAST [
	self
		assert:
			(ClyAddConditionalBreakpointCommand new
				conditionBlockProducerMethodAST: 'thisContext')
		equals:
			(self parserClass
				parseMethod:
					'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext ]')
]
