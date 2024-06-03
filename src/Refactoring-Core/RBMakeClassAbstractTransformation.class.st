"
I am a transformation that turns class into abstract class. This operation is a transformation and not a refactoring because we cannot easily validate that the class is actually not used or when used that it is not receiving a message new.
It just adds class side method `isAbstract` to the selected class, and as such cannot break program. 
So it is behavior preserving but we prefer to stress the fact that we have no warranty that the class is really abstract.
"
Class {
	#name : 'RBMakeClassAbstractTransformation',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'targetClass'
	],
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'displaying' }
RBMakeClassAbstractTransformation class >> basicMenuItemString [

	^ 'Make abstract'
]

{ #category : 'instance creation' }
RBMakeClassAbstractTransformation class >> class: targetClass [

	^ self new class: targetClass
]

{ #category : 'testing' }
RBMakeClassAbstractTransformation class >> isTransformation [

	^ true
]

{ #category : 'instance creation' }
RBMakeClassAbstractTransformation >> class: class [

	targetClass := class
]

{ #category : 'preconditions' }
RBMakeClassAbstractTransformation >> preconditions [
	"We cannot validate that the class is actually not used and not receiving a message new."
	
	^ self skippingPreconditions
]

{ #category : 'transforming' }
RBMakeClassAbstractTransformation >> privateTransform [

	self generateChangesFor: (RBAddMethodTransformation
		 sourceCode: 'isAbstract

	^ self == ' , targetClass asString
		 in: targetClass classSide
		 withProtocol: #testing) execute
]

{ #category : 'preconditions' }
RBMakeClassAbstractTransformation >> skippingPreconditions [
	"We cannot validate that the class is actually not used and not receiving a message new."
	
	^ (RBCondition isAbstractClass: targetClass) not
]
