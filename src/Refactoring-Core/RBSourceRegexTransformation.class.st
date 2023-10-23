"
I am a regex refactoring replacing method sources.

See comment of superclass for a nice script to be adapated to method sources.
"
Class {
	#name : 'RBSourceRegexTransformation',
	#superclass : 'RBRegexRefactoring',
	#category : 'Refactoring-Core-Transformation-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Transformation-Unused'
}

{ #category : 'private' }
RBSourceRegexTransformation >> parseMethod: aString [
	^ [ self parserClass parseMethod: aString ]
		on: Error
		do: [ :err | nil ]
]

{ #category : 'private' }
RBSourceRegexTransformation >> parseSelector: aSelector [
	^ [ self parserClass parseMethodPattern: aSelector ]
		on: Error
		do: [ nil ]
]

{ #category : 'transforming' }
RBSourceRegexTransformation >> privateTransform [

	| original replacement protocols |

	self model
		allClassesDo: [ :class |
			class selectors
				do: [ :selector |
					original := class sourceCodeFor: selector.
					replacement := self execute: original.
					replacement = original
						ifFalse: [ ( self parseMethod: replacement )
								ifNotNil: [ protocols := class protocolsFor: selector.
									( self parseSelector: replacement ) = selector
										ifFalse: [ self generateChangesFor: 
											(RBRemoveMethodTransformation
												selector: selector
												from: class) ].
										self generateChangesFor:
											(RBAddMethodTransformation
												sourceCode: replacement
												in: class
												withProtocol: protocols)
									]
							]
					]
			]
]
