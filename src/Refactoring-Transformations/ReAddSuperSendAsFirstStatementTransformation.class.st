"
I am a small transformation that adds super call to given method as a first statement.

You can use me like this:
```
ReAddSuperSendAsFirstStatementTransformation 
	model: self model
	methodTree: method
	inClass: class) execute
```
I will take `method`'s selector and do `super methodsSelector` as first statement.

I am used to add super call to setUp method for example, I can be also used to add missing `initialize` super calls.
"
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
