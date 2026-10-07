"
I represent a trait composition in a refactoring model by its expression.
My traits are model entities, including traits not yet installed in the image.
I am not a runtime trait composition; executable changes resolve my expression
only after the referenced traits have been created.
"
Class {
	#name : 'RBTraitComposition',
	#superclass : 'Object',
	#instVars : [ 'model', 'expression' ],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBTraitComposition class >> model: aModel expression: anExpression [

	^ self new
		model: aModel;
		expression: anExpression;
		yourself
]

{ #category : 'accessing' }
RBTraitComposition >> expression: anExpression [

	expression := anExpression
]

{ #category : 'accessing' }
RBTraitComposition >> model: aModel [

	model := aModel
]

{ #category : 'printing' }
RBTraitComposition >> asString [

	^ expression
]

{ #category : 'comparing' }
RBTraitComposition >> = anotherComposition [

	^ anotherComposition class = self class and: [
		  anotherComposition expression = expression and: [
			  anotherComposition model = model ] ]
]

{ #category : 'comparing' }
RBTraitComposition >> hash [

	^ expression hash bitXor: model hash
]

{ #category : 'printing' }
RBTraitComposition >> printOn: aStream [

	aStream nextPutAll: expression
]

{ #category : 'accessing' }
RBTraitComposition >> traitCompositionExpression [

	^ expression
]

{ #category : 'accessing' }
RBTraitComposition >> traits [

	^ (OCParser parseExpression: expression) variableNodes
		collect: [ :node | model classNamed: node name ]
]
