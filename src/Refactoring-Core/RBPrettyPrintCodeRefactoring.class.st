"
I am a refactoring for reformat the source code of all methods in this environment.

I have no precondition.
"
Class {
	#name : #RBPrettyPrintCodeRefactoring,
	#superclass : #RBRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBPrettyPrintCodeRefactoring >> preconditions [
	^ RBCondition empty
]

{ #category : #transforming }
RBPrettyPrintCodeRefactoring >> transform [
	| source tree formatted |
	self model
		allClassesDo: [ :class | 
			class selectors
				do: [ :selector | 
					(self model environment includesSelector: selector in: class realClass)
						ifTrue: [ source := class sourceCodeFor: selector.
							source
								ifNotNil: [ tree := class parseTreeFor: selector.
									tree
										ifNotNil: [ formatted := tree formattedCode.
											(source ~= formatted
												and: [ (self parserClass parseMethod: formatted) = tree ])
												ifTrue:
													[ class compile: formatted classified: (class protocolsFor: selector) ] ] ] ] ] ]
]
