"
I am a regex refactoring renaming protocol names.

See comment of superclass for a nice script to be adapated to package names.
"
Class {
	#name : #RBProtocolRegexTransformation,
	#superclass : #RBRegexRefactoring,
	#category : #'Refactoring-Core-Transformation-Unused'
}

{ #category : #transforming }
RBProtocolRegexTransformation >> transform [

	| replacement |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			(class realClass protocolNameOfSelector: selector) asString ifNotNil: [ :original |
				replacement := self execute: original.
				replacement = original ifFalse: [ class compile: (class sourceCodeFor: selector) classified: replacement ] ] ] ]
]
