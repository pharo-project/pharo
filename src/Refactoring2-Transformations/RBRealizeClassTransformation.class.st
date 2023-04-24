"
Class: RBRealizeClassTransformation
                                                                                                    
Complete the set of defined methods of this class, by generating a ""self shouldBeImplemented"" method for all abstract methods defined in its superclass hierarchy. Where an abstract method is a method sending ""self subclassResponsibilty.
Shows a warning if this class has abstract methods on its own.
"
Class {
	#name : #RBRealizeClassTransformation,
	#superclass : #RBClassTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #displaying }
RBRealizeClassTransformation class >> basicMenuItemString [

	^ 'Realize class'
]

{ #category : #api }
RBRealizeClassTransformation class >> className: aName [
	^ self new className: aName
]

{ #category : #testing }
RBRealizeClassTransformation class >> isTransformation [

	^ true
]

{ #category : #api }
RBRealizeClassTransformation class >> model: aRBModel className: aName [
	^ self new
		model: aRBModel;
		className: aName;
		yourself
]

{ #category : #preconditions }
RBRealizeClassTransformation >> preconditions [
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

{ #category : #executing }
RBRealizeClassTransformation >> privateTransform [
	self transform: self theClass.
	self transform: self theClass classSide
]

{ #category : #printing }
RBRealizeClassTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
RBRealizeClassTransformation >> theClass [
	^ (self model classObjectFor: className) instanceSide
]

{ #category : #accessing }
RBRealizeClassTransformation >> transform: targetClass [
	| aClass method parseTree  |
	targetClass allSelectors do: [ :selector |
		aClass := targetClass whoDefinesMethod: selector.
		(aClass notNil and: [ aClass ~= targetClass ]) ifTrue: [
			method := aClass methodFor: selector.
			(method notNil and: [ method refersToSymbol: #subclassResponsibility ]) ifTrue: [
				parseTree := method parseTree.
				parseTree body
					temporaries: OrderedCollection new;
					statements: OrderedCollection new;
					addNode: (RBMessageNode receiver: RBVariableNode selfNode selector: #shouldBeImplemented).
				targetClass compile: parseTree newSource withAttributesFrom: method ] ] ]
]
