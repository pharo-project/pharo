Class {
	#name : 'ReAddSuperSendAsFirstStatementTransformation',
	#superclass : 'RBMethodTransformation',
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'instance creation' }
ReAddSuperSendAsFirstStatementTransformation class >> model: aRBNamespace methodTree: aRBMethodNode inClass: aRBClass [
	^ self new
		model: aRBNamespace ;
		methodTree: aRBMethodNode inClass: aRBClass
]

{ #category : 'initialization' }
ReAddSuperSendAsFirstStatementTransformation >> methodTree: aRBMethodNode inClass: aRBClass [ 

	method := aRBMethodNode.
	selector := aRBMethodNode selector.
	class := aRBClass 
]

{ #category : 'transforming' }
ReAddSuperSendAsFirstStatementTransformation >> privateTransform [ 

	| superSend |
	superSend := RBMessageNode receiver: RBVariableNode superNode selector: selector.
	method body addNodeFirst: superSend.
	class compileTree: method
]
