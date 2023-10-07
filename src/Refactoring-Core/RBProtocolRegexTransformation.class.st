"
I am a regex refactoring renaming protocol names.

See comment of superclass for a nice script to be adapated to package names.
"
Class {
	#name : 'RBProtocolRegexTransformation',
	#superclass : 'RBRegexRefactoring',
	#category : 'Refactoring-Core-Transformation-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Transformation-Unused'
}

{ #category : 'transforming' }
RBProtocolRegexTransformation >> privateTransform [

	| replacement |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			(class realClass protocolNameOfSelector: selector) asString ifNotNil: [ :original |
				replacement := self execute: original.
				replacement = original ifFalse: [ self generateChangesFor:
																(RBAddMethodTransformation
																	sourceCode: (class sourceCodeFor: selector)
																	in: class
																	withProtocol: replacement)] ] ] ]
]
