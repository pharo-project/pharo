"
I am a regex refactoring renaming class categories (package names).

See comment of superclass for a nice script to be adapated to package names.
"
Class {
	#name : #RBCategoryRegexRefactoring,
	#superclass : #RBRegexRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBCategoryRegexRefactoring >> transform [
	| replacement |
	self model allClassesDo: [ :class |
		(class isNil or: [ class isMeta ]) ifFalse: [
			replacement := self execute: class category.
			replacement = class category asString ifFalse: [
				class category: replacement.
				self model defineClass: class definitionString ] ] ]
]
