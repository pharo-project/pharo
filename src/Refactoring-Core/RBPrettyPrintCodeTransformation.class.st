"
I am a refactoring for reformat the source code of all methods in this environment.

I have no precondition.
"
Class {
	#name : #RBPrettyPrintCodeTransformation,
	#superclass : #RBRefactoring,
	#category : #'Refactoring-Core-Transformation-Unused'
}

{ #category : #preconditions }
RBPrettyPrintCodeTransformation >> preconditions [

	^ self emptyCondition
]

{ #category : #transforming }
RBPrettyPrintCodeTransformation >> transform [
	| source tree formatted |
	self model
		allClassesDo: [ :class |
			class selectors
				do: [ :selector |
					(self model environment includesSelector: selector in: class realClass)
						ifTrue: [ source := class sourceCodeFor: selector.
							source
								ifNotNil: [ tree := class parseTreeForSelector: selector.
									tree
										ifNotNil: [ formatted := tree formattedCode.
											(source ~= formatted
												and: [ (self parserClass parseMethod: formatted) = tree ])
												ifTrue:
													[ class compile: formatted classified: (class protocolsFor: selector) ] ] ] ] ] ]
]
