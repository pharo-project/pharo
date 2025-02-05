"
I add Reflectivity link analysis to the Semantic analyzer.

I add #beforeHooks and #afterHooks anotation and make sure to visit them for semantic analysis.
"
Class {
	#name : 'RFSemanticAnalyzer',
	#superclass : 'OCASTSemanticAnalyzer',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'visiting' }
RFSemanticAnalyzer >> analyseForLinks: aNode generator: aGenerator [

	aNode propertyAt: #preambles put: aGenerator preamble.
	aNode preambles do: [:each | self visitNode: each].

	aNode hasMetalinkBefore ifTrue: [
		aNode propertyAt: #beforeHooks put: aGenerator beforeHooks.
 		aNode beforeHooks do: [:each | self visitNode: each]].

	aNode hasMetalinkAfter ifTrue: [
		aNode propertyAt: #postambles put: aGenerator postamble.
		aNode postambles do: [:each | self visitNode: each].
		aNode propertyAt: #afterHooks put: aGenerator afterHooks.
		aNode afterHooks do: [:each | self visitNode: each]].

	aNode hasMetalinkInstead ifTrue: [
		aNode propertyAt: #insteadHooks put: aGenerator insteadHooks.
 		aNode insteadHooks do: [:each | self visitNode: each]]
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> analyseForLinksForNodes: aNode [
	| generator |
	aNode hasMetalink
		ifFalse: [ ^ self ].
	generator := HookGenerator node: aNode.
	self analyseForLinks: aNode generator: generator
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> analyseForLinksForVariables: aVariableNode [
	| variable generator |
	variable := aVariableNode binding.
	variable hasMetalink
		ifFalse: [ ^ self ].
	generator := HookGeneratorForVariables entity: variable node: aVariableNode.
	self analyseForLinks: variable generator: generator
]

{ #category : 'api' }
RFSemanticAnalyzer >> analyze: aNode [
	self visitNode: aNode.
	RFASTClosureAnalyzer new visitNode: aNode
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitAssignmentNode: aNode [
	self visitVarWrite: aNode variable.
	super visitAssignmentNode: aNode
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitMethodNode: aMethodNode [

	scope := OCMethodScope new outerScope: self outerScope.
	aMethodNode scope: scope.  scope node: aMethodNode.
	aMethodNode arguments do: [:node | self declareArgumentNode: node ].
	aMethodNode pragmas do: [:each | self visitNode: each].
	(aMethodNode hasProperty: #wrapperMethod)
		ifTrue: [self declareTemporaryNode: (OCVariableNode named: #RFReifyValueVar)].
	self analyseForLinksForNodes: aMethodNode.
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitNode: aNode [
	"here we add the hook anotations and visit those AST parts for semantic analysis"
	super visitNode: aNode.
	aNode isMethod ifFalse: [self analyseForLinksForNodes: aNode]
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitStoreIntoTempNode: aNode [
	| name var |
	name := aNode name.
	var := scope lookupVar: name.
	var	ifNil: [
			var := scope addTemp: (TemporaryVariable named: aNode name) ].
	aNode binding: var
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitStorePopIntoTempNode: aNode [
	| name var |
	name := aNode name.
	var := scope lookupVar: name.
	var	ifNil: [
			var := scope addTemp: (TemporaryVariable named: aNode name) ].
	aNode binding: var
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitVarWrite: aNode [
	self analyseForLinksForNodes: aNode
]

{ #category : 'visiting' }
RFSemanticAnalyzer >> visitVariableNode: aVariableNode [
	super visitVariableNode: aVariableNode.
	self analyseForLinksForVariables: aVariableNode
]
