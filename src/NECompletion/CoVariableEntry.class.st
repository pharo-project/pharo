"
my subclasses model pssible completions for variables
"
Class {
	#name : 'CoVariableEntry',
	#superclass : 'CoEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'ui related' }
CoVariableEntry >> browseCompletionItem [

	| class |
	class := self classOrNil.
	class ifNotNil: [ (Smalltalk tools toolNamed: #browser) openOnClass: class ].
	^ true
]

{ #category : 'detail information' }
CoVariableEntry >> classOrNil [

	^ self class environment at: contents ifAbsent: [ nil ].

]

{ #category : 'detail information' }
CoVariableEntry >> createDescription [

	| clazz |
	clazz := self classOrNil.
	^ clazz 
		ifNil: [ CoEntryDescription label: self label ]
		ifNotNil: [
			clazz isClass
				ifFalse: [ "we have few globals but we should handle them correctly" 
					CoEntryDescription label: self label ]
				ifTrue: [ 
					CoEntryDescription 
						label: self label 
						title: clazz printString 
						description: clazz comment ] ]
]
