"
I am a refactoring for reformat the source code of all methods in this environment.

I have no precondition.
"
Class {
	#name : 'RBPrettyPrintCodeTransformation',
	#superclass : 'ReRefactoring',
	#category : 'Refactoring-Core-Transformation-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Transformation-Unused'
}

{ #category : 'transforming' }
RBPrettyPrintCodeTransformation >> privateTransform [
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
													[ 	self generateChangesFor:
															(RBAddMethodTransformation
																sourceCode: formatted
																in: class
																withProtocol: (class protocolsFor: selector)) ] ] ] ] ] ]
]
