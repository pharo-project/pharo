"
Complete the set of defined methods of this class, by generating a ""self shouldBeImplemented"" method for all abstract methods defined in its superclass hierarchy. Where an abstract method is a method sending ""self subclassResponsibilty.
Shows a warning if this class has abstract methods on its own.
"
Class {
	#name : #RBRealizeClassRefactoring,
	#superclass : #RBClassRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBRealizeClassRefactoring >> preconditions [
	^ RBCondition
		withBlock: [ 
			(self theClass withAllSubclasses
				detect: [ :each | 
					(each whichSelectorsReferToSymbol: #subclassResponsibility) notEmpty 
						or: [ (each classSide whichSelectorsReferToSymbol: #subclassResponsibility) notEmpty ] ]
				ifNone: [ nil ]) ifNotNil: 
				[ self refactoringWarning: 
				self theClass printString ,
					' is abstract or has abstract subclasses. Do you want to proceed?']. true ]
]

{ #category : #printing }
RBRealizeClassRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
RBRealizeClassRefactoring >> theClass [
	^ (self classObjectFor: className) instanceSide
]

{ #category : #transforming }
RBRealizeClassRefactoring >> transform [
	self transform: self theClass.
	self transform: self theClass classSide
]

{ #category : #transforming }
RBRealizeClassRefactoring >> transform: aClass [
	| class method parseTree |
	aClass allSelectors do: [ :selector | 
		class := aClass whoDefinesMethod: selector.
		(class notNil and: [ class ~= aClass ]) ifTrue: [ 
			method := class methodFor: selector.
			(method notNil and: [ method refersToSymbol: #subclassResponsibility ]) ifTrue: [ 
				parseTree := method parseTree.
				parseTree body
					temporaries: OrderedCollection new;
					statements: OrderedCollection new;
					addNode: (RBMessageNode receiver: RBVariableNode selfNode selector: #shouldBeImplemented).
				aClass compile: parseTree newSource withAttributesFrom: method ] ] ]
]
