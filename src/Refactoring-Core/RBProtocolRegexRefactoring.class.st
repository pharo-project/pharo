"
I am a regex refactoring renaming protocol names.
"
Class {
	#name : #RBProtocolRegexRefactoring,
	#superclass : #RBRegexRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBProtocolRegexRefactoring >> transform [
	| original replacement |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			original := (class realClass whichCategoryIncludesSelector: selector)
				asString.
			original isNil ifFalse: [
				replacement := self execute: original.
				replacement = original ifFalse: [
					class
						compile: (class sourceCodeFor: selector)
						classified: replacement ] ] ] ]
]
