"
my subclasses model pssible completions for variables
"
Class {
	#name : #NECCVariableEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #'ui related' }
NECCVariableEntry >> browse [
	| class |
	class := Smalltalk globals at: contents ifAbsent: [ nil ].
	class
		ifNotNil: [ Smalltalk tools browser openOnClass: class].
	^ true
]

{ #category : #'detail information' }
NECCVariableEntry >> createDescription [
	| clazz |
	clazz := Smalltalk globals at: contents ifAbsent: [ nil ].
	^ clazz 
		ifNil: [ NECEntryDescription label: self label ]
		ifNotNil: 
			[ NECEntryDescription 
				label: self label
				title: clazz printString
				description: clazz comment ]
]
