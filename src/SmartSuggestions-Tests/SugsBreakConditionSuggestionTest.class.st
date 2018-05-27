Class {
	#name : #SugsBreakConditionSuggestionTest,
	#superclass : #TestCase,
	#category : #'SmartSuggestions-Tests'
}

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testMessageSend_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'val msg: arg')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ((ThisContext lookupSymbol: #val) msg: (ThisContext lookupSymbol: #arg))]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testSelf_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'self')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext receiver ]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testSimpleCondition_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'true')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: true ]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testSuperSendNoArg_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'super msg')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #msg withArguments: {} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testSuperSendWithArgs_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'super foo: arg1 bar: arg2')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext receiver perform: #foo:bar: withArguments: {ThisContext lookupSymbol: #arg1. ThisContext lookupSymbol: #arg2.} inSuperclass: ThisContext receiver class superclass)]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testVariableLookup_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'var')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #var)]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testVariableNamedThisContext_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'ThisContext')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: (ThisContext lookupSymbol: #ThisContext)]')
]

{ #category : #tests }
SugsBreakConditionSuggestionTest >> testthisContext_conditionBlockProducerMethodAST [
	self 
		assert:
			(SugsBreakConditionSuggestion new conditionBlockProducerMethodAST: 'thisContext')
		equals:
			(RBParser parseMethod: 'conditionBlockGenerator ^[ :ThisContext | Breakpoint checkBreakConditionValue: ThisContext ]')
]
