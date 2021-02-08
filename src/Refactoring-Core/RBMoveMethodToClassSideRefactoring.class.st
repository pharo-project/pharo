Class {
	#name : #RBMoveMethodToClassSideRefactoring,
	#superclass : #RBMoveMethodToClassRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBMoveMethodToClassSideRefactoring >> addMethod: rbMethod to: aClass toProtocol: protocol [
	aClass addMethod: rbMethod.
	aClass compile: rbMethod source classified: {protocol}.
]

{ #category : #creating }
RBMoveMethodToClassSideRefactoring >> metaClassOf: aClass [
	| classModel |
	classModel :=RBClassModelFactory rbMetaclass named: aClass name.
	classModel model: self model.
	^classModel
]

{ #category : #initialization }
RBMoveMethodToClassSideRefactoring >> method: aMethod class: aClass [
	method := aMethod.
	class := self classObjectFor: aClass.
]

{ #category : #preconditions }
RBMoveMethodToClassSideRefactoring >> preconditions [
	^(RBCondition definesSelector: method selector in: (self metaClassOf: class)) not
		& (RBCondition withBlock: [ class isMeta not ])
]

{ #category : #transforming }
RBMoveMethodToClassSideRefactoring >> transform [
	| oldClass newClass rbMethod rbMethod2 newSource originalProtocol |
	oldClass := class.
	newClass := self metaClassOf: class.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	newSource := method selector, '
	^ self class ',  method selector.
	rbMethod2 := RBClassModelFactory rbMethod for: oldClass source: newSource selector: method selector.
	originalProtocol := method protocol.
	oldClass removeMethod: method selector.
	self addMethod: rbMethod to: newClass toProtocol: originalProtocol.
	self addMethod: rbMethod2 to: oldClass toProtocol: originalProtocol.
]
