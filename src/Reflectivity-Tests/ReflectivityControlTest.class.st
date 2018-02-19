"
Tests for Reflective method
"
Class {
	#name : #ReflectivityControlTest,
	#superclass : #TestCase,
	#instVars : [
		'tag',
		'link'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #tagging }
ReflectivityControlTest >> return3 [
	^3
]

{ #category : #tagging }
ReflectivityControlTest >> tagExec [
	tag := #yes
]

{ #category : #tagging }
ReflectivityControlTest >> tagExec: aTag [
	tag := aTag.
]

{ #category : #running }
ReflectivityControlTest >> tearDown [
	link ifNotNil: [ link uninstall ].
	super tearDown 
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterArray [
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleArray isArray.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterAssignment [
	| assignmentNode |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	self assert: assignmentNode isAssignment.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	assignmentNode link: link.
	self assert: assignmentNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterBlock [
	| blockNode |
	blockNode := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver.
	self assert: blockNode isBlock.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	blockNode link: link.
	self assert: blockNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterBlockSequence [
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	sequence link: link.
	self assert: sequence hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - after - variables' }
ReflectivityControlTest >> testAfterClassVariable [
	| classVar |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	classVar link: link.
	self assert: classVar hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleClassVarRead  = #AClassVar.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleClassVarRead) isQuick.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterLiteral [
	| literalNode |
	literalNode := (ReflectivityExamples >> #exampleLiteral) ast statements first value.
	self assert: literalNode isLiteralNode.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	literalNode link: link.
	self assert: literalNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteral == 2.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterLiteralArray [
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleLiteralArray) ast statements first value.
	self assert: literalArray isLiteralArray.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	literalArray link: link.
	self assert: literalArray hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteralArray = #(1).
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteralArray) isQuick.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterMethod [
	| methodNode |
	methodNode := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterMethodPrimitive [
	| methodNode |
	ReflectivityExamples recompile: #examplePrimitiveMethod.
	methodNode := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new examplePrimitiveMethod class = ByteString.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterMethodWithTemps [
	| methodNode |
	ReflectivityExamples recompile: #exampleAssignment.
	methodNode := (ReflectivityExamples >> #exampleAssignment) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment  = 3.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterReturn [
	| returnNode |
	returnNode := (ReflectivityExamples >> #exampleMethod) ast statements first.
	self assert: returnNode isReturn.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	self should: [returnNode link: link] raise: Error.
	
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterSend [
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	self assert: sendNode isMessage.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		control: #after;
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterSendWeak [
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	self assert: sendNode isMessage.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		control: #after;
		option: #(+ optionWeakAfter);
		arguments: #(node).
	sendNode link: link.
	self assert: sendNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	self deny: ((ReflectivityExamples >> #exampleMethod) messages includes: #ensure:).
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterSequence [
	| sequence |
	sequence := (ReflectivityExamples >> #exampleMethod) ast body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	sequence link: link.
	self assert: sequence hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - after - variables' }
ReflectivityControlTest >> testAfterSlot [
	| iVar instance |
	iVar := ReflectivityExamples slotNamed: #ivar.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		control: #after;
		arguments: #(name).
	iVar link: link.
	self assert: iVar hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleIvarRead = 33).
	self assert: (tag = #ivar).
]

{ #category : #'tests - after' }
ReflectivityControlTest >> testAfterVariableNode [
	| variableNode |
	variableNode := (ReflectivityExamples >> #exampleGlobalRead) ast statements first value.
	self assert: variableNode isVariable.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	variableNode link: link.
	self assert: variableNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleGlobalRead = GlobalForTesting.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleGlobalRead) isQuick.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeAssignment [
	| assignmentNode |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	assignmentNode link: link.
	self assert: assignmentNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeBlock [
	| blockNode |
	blockNode := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver.
	self assert: blockNode isBlock.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	blockNode link: link.
	self assert: blockNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeBlockSequence [
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	sequence link: link.
	self assert: sequence hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeBlockSequenceNoValue [
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlockNoValue) ast statements first value body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	sequence link: link.
	self assert: sequence hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleBlockNoValue) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleBlockNoValue.
	self assert: tag isNil.
	ReflectivityExamples new exampleBlockNoValue value.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlockNoValue) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeClassVariable [

	| classVar |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	classVar link: link.
	self assert: classVar hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleClassVarRead = #AClassVar.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleClassVarRead) isQuick.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeLiteral [
	| literalNode |
	literalNode := (ReflectivityExamples >> #exampleLiteral) ast statements first value.
	self assert: literalNode isLiteralNode.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	literalNode link: link.
	self assert: literalNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteral == 2.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeLiteralArray [
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleLiteralArray) ast statements first value.
	self assert: literalArray isLiteralArray.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	literalArray link: link.
	self assert: literalArray hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteralArray = #(1).
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteralArray) isQuick.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeMethod [
	| methodNode |
	methodNode := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeMethodPrimitive [
	| methodNode |
	methodNode := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.	
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new examplePrimitiveMethod class = ByteString.
	self deny: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.	
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	link uninstall.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeReturn [
	| returnNode |
	returnNode := (ReflectivityExamples >> #exampleLiteral) ast statements first.
	self assert: returnNode isReturn.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	returnNode link: link.
	self assert: returnNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteral == 2.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeSend [
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeSequence [
	| seqNode |
	seqNode := (ReflectivityExamples >> #exampleAssignment) ast body.
	self assert: seqNode isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	self assert: (seqNode isKindOf: RBSequenceNode).
	seqNode link: link.
	self assert: seqNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testBeforeVariable [
	| variableNode |
	variableNode := (ReflectivityExamples >> #exampleGlobalRead) ast statements first value.
	self assert: variableNode isVariable.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	variableNode link: link.
	self assert: variableNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleGlobalRead = GlobalForTesting.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleGlobalRead) isQuick.
]

{ #category : #'tests - conditions' }
ReflectivityControlTest >> testConditionDisableEnableNoArguments [
"check that a link is not executed, if the condition is disabled (link>>#disable)."
"and  that it is executed again if the condition is enabled again (link>>#enable"
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ true ];
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.
	tag := nil.
	link disable.
	ReflectivityExamples new exampleMethod.
	self assert: tag isNil.
	link enable.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.

]

{ #category : #'tests - conditions' }
ReflectivityControlTest >> testConditionDisableEnableWithArguments [
"check that a link is not executed, if the condition is disabled (link>>#disable)."
"and  that it is executed again if the condition is enabled again (link>>#enable"
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ :arg | arg == sendNode ] arguments:#(#node);
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.
	tag := nil.
	link disable.
	ReflectivityExamples new exampleMethod.
	self assert: tag isNil.
	link enable.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.

]

{ #category : #'tests - conditions' }
ReflectivityControlTest >> testConditionWithArgument [
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ :node | node == sendNode ] arguments: #(#node);
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	link condition: [ :node | node == 5 ].
	tag := nil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag isNil.
]

{ #category : #'tests - conditions' }
ReflectivityControlTest >> testConditionWithArgument2 [
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ :arg | arg == sendNode ] arguments: #(node);
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	link condition: [ :node | node == 5 ].
	tag := nil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag isNil.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testFailingDoubleRWInstead [

	|readLink readMO writeLink writeMO readSites writeSites|

	readSites := (ReflectivityExamples>>#exampleLazyInit) 
		variableReadNodes select: [ :node | node name = 'singleton' ].
		
	writeSites :=  (ReflectivityExamples>>#exampleLazyInit) 
		variableWriteNodes select: [ :node | node name = 'singleton' ].
	
	"default MO for rw -- bug manifests on install"
	readMO := [ :object | object class classVarNamed: 'singleton' ].
	writeMO := [ :object :newValue | object class classVarNamed: 'singleton' put: newValue ].
		
	readLink := MetaLink new 
		metaObject: readMO; 
		selector: #value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(object).
		
	writeLink := MetaLink new 
		metaObject: writeMO; 
		selector: #value:value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(newValue object).
	
	readSites do: [ :readNode | readNode link: readLink ].
	writeSites do: [ :writeNode | writeNode link: writeLink ].
	readLink uninstall.
	writeLink uninstall.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testFailingDoubleRWInsteadSimplified [

	|readLink readMO readNode|

	readNode := ((ReflectivityExamples>>#exampleLazyInit) 
		variableReadNodes select: [ :node | node name = 'singleton' ]) first.

	"default MO for rw -- bug manifests on install"
	readMO := [ :object | object class classVarNamed: 'singleton' ].
		
	readLink := MetaLink new 
		metaObject: readMO; 
		selector: #value:;
		control: #instead;
		options: #(+ optionCompileOnLinkInstallation);
		arguments: #(object).
			
	readNode link: readLink.
	readLink uninstall.

]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadArray [
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	self assert: arrayNode isDynamicArray.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleArray = 3.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleArray) isQuick.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadAssign [
	| assignmentNode instance |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	self assert: assignmentNode isAssignment.
	link := MetaLink new
		metaObject: #context;
		selector: #tempNamed:put:;
		control: #instead;
		arguments: #(#name #newValue).
	assignmentNode link: link.
	self assert: assignmentNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleAssignment = 3.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadBlock [
	| blockNode |
	blockNode := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver.
	self assert: blockNode isBlock.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	self assert: ReflectivityExamples new exampleBlock == 5.
	blockNode link: link.
	self assert: blockNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleBlock == 3.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadBlockSequence [
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	sequence link: link.
	self assert: sequence hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
 	ReflectivityExamples new exampleBlock.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.
]

{ #category : #'tests - after - variables' }
ReflectivityControlTest >> testInsteadClassVariable [
	| classVar |
	self skip.
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	classVar link: link.
	self assert: classVar hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleClassVarRead  = #AClassVar.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleClassVarRead) isQuick.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadLiteral [
	| literalNode |
	literalNode := (ReflectivityExamples >> #exampleLiteral) ast statements first value.
	self assert: literalNode isLiteralNode.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	literalNode link: link.
	self assert: literalNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleLiteral == 3.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadLiteralArray [
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleLiteralArray) ast statements first value.
	self assert: literalArray isLiteralArray.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	literalArray link: link.
	self assert: literalArray hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleLiteralArray = 3.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteralArray) isQuick.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadMethod [
	| node |
	node := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	node link: link.
	self assert: node hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleMethod.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadPrimitiveMethod [
	| node |
	ReflectivityExamples recompile: #examplePrimitiveMethod.
	node := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	node link: link.
	self assert: node hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	ReflectivityExamples new examplePrimitiveMethod.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadReturn [
	| returnNode |
	returnNode := (ReflectivityExamples >> #exampleMethod) ast statements first.
	self assert: returnNode isReturn.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	self should: [returnNode link: link] raise: Error.
	
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadSend [
	| sendNode instance |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: #receiver;
		selector: #perform:withArguments:;
		control: #instead;
		arguments: #(#selector #arguments).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethod = 5.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadSendMultipleSites [
	| sendNodes instance |
	self skip.
	sendNodes := (ReflectivityExamples >> #exampleMethodMultipleSites) sendNodes.
	link := MetaLink new
		metaObject: [:receiver :selector :arguments | receiver perform: selector withArguments: arguments];
		selector: #value:value:value:;
		control: #instead;
		arguments: #(#receiver #selector #arguments).
	sendNodes do: [ :sn | sn link: link].
	self assert: sendNodes anyOne hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethodMultipleSites) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleMethodMultipleSites= 5.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadSequence [
	| seqNode |
	seqNode := (ReflectivityExamples >> #exampleAssignment) ast body.
	self assert: seqNode isSequence.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #tagExec.
	self assert: (seqNode isKindOf: RBSequenceNode).
	seqNode link: link.
	self assert: seqNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleAssignment.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadVariableReadGlobal [
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleGlobalRead) variableReadNodes first.
	link := MetaLink new
		metaObject: #class;
		selector: #bindingOf:;
		control: #instead;
		arguments: #(#name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleGlobalRead value = GlobalForTesting.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadVariableReadIvar [
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleIvarRead) variableReadNodes first.
	link := MetaLink new
		metaObject: #object;
		selector: #instVarNamed:;
		control: #instead;
		arguments: #(#name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleIvarRead) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleIvarRead value = 33.
	self deny: (ReflectivityExamples >> #exampleIvarRead) isQuick.	"yes, we changed the method"
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadVariableReadTemp [
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleAssignment) variableReadNodes first.
	link := MetaLink new
		metaObject: #context;
		selector: #tempNamed:;
		control: #instead;
		arguments: #(#name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleAssignment value = 3.
]

{ #category : #'tests - instead' }
ReflectivityControlTest >> testInsteadVariableWrite [
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleAssignment) variableWriteNodes first.
	link := MetaLink new
		metaObject: #context;
		selector: #tempNamed:put:;
		control: #instead;
		arguments: #(#name #newValue).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleAssignment = 3.
]

{ #category : #'tests - options' }
ReflectivityControlTest >> testLinkOneShot [
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(+ optionOneShot);
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	self deny: sendNode hasMetalink.
	tag := nil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: tag isNil.

]

{ #category : #'tests - options' }
ReflectivityControlTest >> testLinkOptionDisabledLink [
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(+ optionDisabledLink);
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: tag isNil.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - options' }
ReflectivityControlTest >> testLinkoptionInlineCondition [
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(- optionInlineCondition);
		condition: [ true ];
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
		self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	"if we now change the meta object, the method should not be invalidated"
	link condition: [true].
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	tag := nil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self deny: tag isNil.
	link condition: [true].
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	tag := nil.
	link condition: [false].
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: tag isNil.
]

{ #category : #'tests - options' }
ReflectivityControlTest >> testLinkoptionInlineMetaObject [
	| sendNode |
	sendNode := (ReflectivityExamples>>#exampleMethod) sendNodes first.
	link := MetaLink new 
		metaObject: self; 
		options: #(- optionInlineMetaObject);
		selector: #tagExec.
	sendNode link: link.
	self assert: sendNode hasMetalink.
		self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: (ReflectivityExamples new exampleMethod = 5).
	self assert: (tag = #yes).
	"if we now change the meta object, the method should not be invalidated"
	link metaObject: self class new.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
]

{ #category : #'tests - before' }
ReflectivityControlTest >> testbeforeArray [
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	self assert: arrayNode isDynamicArray.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleArray isArray.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.
]
