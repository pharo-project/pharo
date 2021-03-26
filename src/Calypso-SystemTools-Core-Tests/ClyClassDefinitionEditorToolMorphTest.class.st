"
A ClyClassDefinitionEditorToolMorphTest is a test class for testing the behavior of ClyClassDefinitionEditorToolMorph
"
Class {
	#name : #ClyClassDefinitionEditorToolMorphTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinitionEditor',
		'editor',
		'className'
	],
	#category : #'Calypso-SystemTools-Core-Tests-Editors-Classes'
}

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> checkClassDoesntExists [
	(self class environment classNamed: className) ifNotNil: [ self error: 'Need to rename test class to not override existing class' ].
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> classExists [
	^ (self class environment classNamed: className) isNotNil

]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> deleteClassIfExists [
	| instaledClass |
	instaledClass := self class environment classNamed: className.
	instaledClass ifNil:[ ^ self ].
	instaledClass removeFromSystem
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> setUp [
	| context browser containerTab methodGroupQuery |
	super setUp.

	className := #MyClass.
	
	context := MockClyFullBrowserClassContext new.
	methodGroupQuery := MockMethodGroupQuery new.
	browser := (ClyFullBrowserBlockingUI on: ClyNavigationEnvironment currentImage)
				methodGroupQuery: methodGroupQuery;
				yourself.
	containerTab := MockClyContainerTab new.
	editor := ClyClassDefinitionEditorToolMorph new
		browser: nil;
		context: context;
		containerTab: containerTab;
		browser: browser;
		setUpModelFromContext;
		build;
		yourself.
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredDefinitionFailsWithoutTryingToCompileAsMethod [
 	editor textMorph appendText: 'Object << #MyClass
		slots: { #s #s };
    package: ''Calypso-SystemQueries-Tests'''.
	editor applyChanges.

	self error
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredFluidDefinitionFailsWithoutTryingToCompileAsMethod [
	editor textMorph setTextWith: 
	'Object << #', className , '
		slots: { #s #s };
		package: ''Calypso-SystemTools-Core-Tests'''.
	editor applyChanges.
	self assert: editor textMorph text string equals: 
		'Object << #MyClass
		slots: { #s # End of statement expected ->s };
		package: ''Calypso-SystemTools-Core-Tests'''
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredMethodShouldFailCompilationAsMethod [
	"failing compilation of a method definition & a fluid class definition should treat their own syntax errors"
	editor textMorph setTextWith: 
	'banana^'.
	editor applyChanges.
	self assert: editor textMorph text string equals: 
		'banana^ Variable or expression expected ->'
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredTraitDefinitionFailsWithoutTryingToCompileAsMethod [
	editor textMorph setTextWith: 
	'Object << #', className , '
		slots: { #s #s };
		package: ''Calypso-SystemTools-Core-Tests'''.
	editor applyChanges.
	self assert: editor textMorph text string equals: 
		'Object << #MyClass
		slots: { #s # End of statement expected ->s };
		package: ''Calypso-SystemTools-Core-Tests'''.
		
	self error: #todo
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenClassDefinitionRegistersIt [
	self checkClassDoesntExists.
	[
	editor textMorph setTextWith: 
	'Object subclass: ' , className , '
	instanceVariableNames: '' 
	classVariableNames: ''
	package: ''Calypso-SystemTools-Core-Tests'''.
	editor applyChanges.
	self assert: self classExists.
	
	] ensure: [ self deleteClassIfExists ].
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenFluidClassDefinitionRegistersIt [
	self checkClassDoesntExists.
	[
	editor textMorph setTextWith: 
	'Object << #', className , '
		slots: { };
    package: ''Calypso-SystemTools-Core-Tests'''.
	editor applyChanges.
	self assert: self classExists.
	
	] ensure: [ self deleteClassIfExists ].
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenMethodRegistersIt [
	| selector |
	selector := #banana.
	(self class lookupSelector: selector) ifNotNil: [ self error: 'I might overide an important behavior' ].
	[
	editor textMorph setTextWith: 
	selector , '
	^ 1'.
	editor applyChanges.
	self assert: (self class methodDict includes: selector).
	
	] ensure: [ self class removeSelector: selector ].

]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenTraitDefinitionRegistersIt [
	self error: #todo.
 	editor textMorph appendText: 'Object << #MyClass
		slots: { #s #s };
    package: ''Calypso-SystemQueries-Tests'''.
	editor applyChanges .

]
