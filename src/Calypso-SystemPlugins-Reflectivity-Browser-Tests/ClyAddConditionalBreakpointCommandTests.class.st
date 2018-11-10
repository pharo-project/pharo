Class {
	#name : #ClyAddConditionalBreakpointCommandTests,
	#superclass : #TestCase,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Tests-Breakpoints'
}

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testMessageSend_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST:  'val msg: arg')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ((ThisContext lookupSymbol: #val) msg: (ThisContext lookupSymbol: #arg))]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testSelf_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'self')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext receiver ]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testSimpleCondition_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'true')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: true ]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testSuperSendNoArg_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'super msg')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #msg withArguments: {} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testSuperSendWithArgs_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'super foo: arg1 bar: arg2')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #foo:bar: withArguments: {ThisContext lookupSymbol: #arg1. ThisContext lookupSymbol: #arg2.} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testVariableLookup_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'var')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #var)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testVariableNamedThisContext_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'ThisContext')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #ThisContext)]')
]

{ #category : #tests }
ClyAddConditionalBreakpointCommandTests >> testthisContext_conditionBlockProducerMethodAST [
	self 
		assert:
			(ClyAddConditionalBreakpointCommand new conditionBlockProducerMethodAST: 'thisContext')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext ]')
]
