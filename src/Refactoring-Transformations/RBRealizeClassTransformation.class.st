"
Class: RBRealizeClassTransformation
                                                                                                    
Complete the set of defined methods of this class, by generating a ""self shouldBeImplemented"" method for all abstract methods defined in its superclass hierarchy. Where an abstract method is a method sending ""self subclassResponsibilty.
Shows a warning if this class has abstract methods on its own.
"
Class {
	#name : 'RBRealizeClassTransformation',
	#superclass : 'RBClassTransformation',
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'displaying' }
RBRealizeClassTransformation class >> basicMenuItemString [

	^ 'Realize class'
]

{ #category : 'testing' }
RBRealizeClassTransformation class >> isTransformation [

	^ true
]

{ #category : 'preconditions' }
RBRealizeClassTransformation >> applicabilityPreconditions [

	^ { (RBCondition isClassNamed: className definedIn: model) }
]

{ #category : 'executing' }
RBRealizeClassTransformation >> privateTransform [
	self transform: self theClass.
	self transform: self theClass classSide
]

{ #category : 'storing' }
RBRealizeClassTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
RBRealizeClassTransformation >> theClass [
	^ (self model classObjectFor: className) instanceSide
]

{ #category : 'accessing' }
RBRealizeClassTransformation >> transform: aClass [
	"Transform all the subclassResponsibilities (inherited or not) methods of the class in the class."

	| definingClass method parseTree |
	"get all the methods even the ones of the superclasses"
	aClass allSelectors do: [ :selector |
		definingClass := aClass whichClassIncludesSelector: selector.
		(definingClass isNotNil and: [ definingClass ~= aClass ]) ifTrue: [
			method := definingClass methodFor: selector.
			(method isNotNil and: [
				 method refersToSymbol: #subclassResponsibility ]) ifTrue: [
				parseTree := method parseTree.
				parseTree body
					temporaries: OrderedCollection new;
					statements: OrderedCollection new;
					addNode: (OCMessageNode
							 receiver: OCVariableNode selfNode
							 selector: #shouldBeImplemented).
				"Note that we compile in the class (a subclass compared to superclass having abstract methods) 
				so we are doing a kind of copy-down but transforming a self subclassResp into shouldBe"
				aClass compile: parseTree newSource withAttributesFrom: method ] ] ]
]
