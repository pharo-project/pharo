"
my subclasses model pssible completions for variables
"
Class {
	#name : 'NECVariableEntry',
	#superclass : 'NECEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'ui related' }
NECVariableEntry >> browse [
	| class |
	class := Smalltalk globals at: contents ifAbsent: [ nil ].
	class
		ifNotNil: [ (Smalltalk tools toolNamed: #browser) openOnClass: class].
	^ true
]

{ #category : 'detail information' }
NECVariableEntry >> createDescription [
	| clazz |
	clazz := self class environment at: contents ifAbsent: [ nil ].
	^ clazz
		ifNil: [ NECEntryDescription label: self label ]
		ifNotNil:
			[ 
			clazz isClass 
				ifFalse: [ 
					"we have few globals but we should handle them correctly"
					NECEntryDescription label: self label ]
				ifTrue: [ NECEntryDescription
					label: self label
					title: clazz printString
					description: clazz comment ]]
]
