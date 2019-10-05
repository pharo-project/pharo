"
I am a regex refactoring renaming class names.
I offer several models of operation in addition to regex matching. 

Refactored classes can be renamed, copied and kept aside old ones: try renameClasses, copyClasses, or createClasses. 

See also the comment of superclass for a nice script.
"
Class {
	#name : #RBClassRegexRefactoring,
	#superclass : #RBRegexRefactoring,
	#instVars : [
		'rootClass',
		'mode'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBClassRegexRefactoring >> copy: aClass name: aSymbol [
	^ self duplicate: aClass name: aSymbol deep: true
]

{ #category : #actions }
RBClassRegexRefactoring >> copyClasses [
	mode := #copy:name:
]

{ #category : #private }
RBClassRegexRefactoring >> copyFrom: aSourceClass to: aTargetClass [
	aSourceClass instanceVariableNames
		do: [ :each | aTargetClass addInstanceVariable: each ].
	aSourceClass isMeta ifFalse: [
		aSourceClass allClassVariableNames
			do: [ :each | aTargetClass addClassVariable: each ].
		aSourceClass sharedPoolNames
			do: [ :each | aTargetClass addPoolDictionary: each ] ].
	aSourceClass selectors do: [ :each |
		aTargetClass
			compile: (aSourceClass sourceCodeFor: each)
			classified: (aSourceClass protocolsFor: each) ]
]

{ #category : #transforming }
RBClassRegexRefactoring >> create: aClass name: aSymbol [
	^ self duplicate: aClass name: aSymbol deep: false
]

{ #category : #actions }
RBClassRegexRefactoring >> createClasses [
	mode := #create:name:
]

{ #category : #private }
RBClassRegexRefactoring >> duplicate: aClass name: aSymbol deep: aBoolean [
	| superclass superclassName name class |
	(self model includesClassNamed: aSymbol)
		ifTrue: [ ^ nil ].
	superclass := aClass superclass 
		ifNil: [ self rootClass ].
	superclassName := (self model includesClassNamed: superclass name)
		ifFalse: [ superclass name ]
		ifTrue: [ 
			(name := self execute: superclass name) = superclass name
				ifFalse: [ self duplicate: superclass name: name deep: aBoolean ].
			name ].
	self model defineClass: ('<1s>
		subclass: #<2s>
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: <3p>' 
			expandMacrosWith: superclassName 
			with: aSymbol with: aClass category asString).
	aBoolean ifTrue: [
		(class := self model classNamed: aSymbol) 
			ifNil: [ ^ self ].
		self copyFrom: aClass to: class.
		self copyFrom: aClass classSide to: class classSide ].
	^ nil
]

{ #category : #initialization }
RBClassRegexRefactoring >> initialize [
	super initialize.
	self createClasses
]

{ #category : #transforming }
RBClassRegexRefactoring >> rename: aClass name: aSymbol [
	^ RBRenameClassRefactoring 
		model: self model
		rename: aClass
		to: aSymbol
]

{ #category : #actions }
RBClassRegexRefactoring >> renameClasses [
	mode := #rename:name:
]

{ #category : #accessing }
RBClassRegexRefactoring >> rootClass [
	^ rootClass ifNil: [ Object ]
]

{ #category : #initialization }
RBClassRegexRefactoring >> rootClass: aClass [
	rootClass := aClass instanceSide
]

{ #category : #transforming }
RBClassRegexRefactoring >> transform [
	| replacement refactoring |
	self model allClassesDo: [ :class |
		(class isNil or: [ class isMeta ]) ifFalse: [
			replacement := self execute: class name.
			replacement = class name asString ifFalse: [
				refactoring := self perform: mode
					with: class with: replacement asSymbol.
				(refactoring notNil and: [ refactoring preconditions check ])
					ifTrue: [ refactoring transform ] ] ] ]
]
