"
A ClyClassDefinitionEditorToolMorphTest is a test class for testing the behavior of ClyClassDefinitionEditorToolMorph
"
Class {
	#name : #ClyClassDefinitionEditorToolMorphTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinitionEditor',
		'editor',
		'className',
		'context'
	],
	#category : #'Calypso-SystemTools-Core-Tests-Editors-Classes'
}

{ #category : #asserting }
ClyClassDefinitionEditorToolMorphTest >> assertResultCompilationEquals: aString [
	self assert: editor textMorph text string equals: aString
]

{ #category : #helper }
ClyClassDefinitionEditorToolMorphTest >> checkClassDoesntExists [
	(self class environment classNamed: className) ifNotNil: [ self error: 'Need to rename test class to not override existing class' ].
]

{ #category : #helper }
ClyClassDefinitionEditorToolMorphTest >> classExists [
	^ (self class environment classNamed: className) isNotNil

]

{ #category : #helper }
ClyClassDefinitionEditorToolMorphTest >> compileString: aString [
	editor textMorph setTextWith: aString.
	editor applyChanges.
]

{ #category : #helper }
ClyClassDefinitionEditorToolMorphTest >> deleteClassIfExists [
	| instaledClass |
	instaledClass := self class environment classNamed: className.
	instaledClass ifNil:[ ^ self ].
	instaledClass removeFromSystem
]

{ #category : #running }
ClyClassDefinitionEditorToolMorphTest >> setUp [
	| browser containerTab methodGroupQuery |
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

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredClassDefinitionFailsWithoutTryingToCompileAsMethod [
	"Not sure what a syntax error look like for a  shift class builder definition.
	Open to ideas"
	self error.
	
	self compileString: 
'Object subclass: #' , className , '
	instanceVariableNames: ''#'' 
	classVariableNames: ''''
  package: ''Calypso-SystemQueries-Tests'''.

	self assertResultCompilationEquals:
'Object subclass: #MyClass
	instanceVariableNames: '''' 
	classVariableNames: ''''
  package: ''Calypso-SystemQueries-Tests'''
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredFluidClassDefinitionFailsWithoutTryingToCompileAsMethod [
	self compileString:
'Object << #', className , '
	slots: { #s #s };
	package: ''Calypso-SystemTools-Core-Tests'''.
	
	self assertResultCompilationEquals:
'Object << #MyClass
	slots: { #s # End of statement expected ->s };
	package: ''Calypso-SystemTools-Core-Tests'''
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredMethodShouldFailCompilationAsMethod [
	"failing compilation of a method definition & a fluid class definition should treat their own syntax errors"
	self compileString: 'banana^'.
	self assertResultCompilationEquals:
		'banana^ Variable or expression expected ->'
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptASyntaxErroredTraitDefinitionFailsWithoutTryingToCompileAsMethod [
	self compileString:
'Object << #', className , '
	slots: { #s #s };
	package: ''Calypso-SystemTools-Core-Tests'''.

	self assertResultCompilationEquals:
'Object << #MyClass
	slots: { #s # End of statement expected ->s };
	package: ''Calypso-SystemTools-Core-Tests'''.
		
	self error: #todo
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenClassDefinitionRegistersIt [
	self checkClassDoesntExists.
	[
	self compileString:
	'Object subclass: #' , className , '
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Calypso-SystemTools-Core-Tests'''.
	self assert: self classExists.
	
	] ensure: [ self deleteClassIfExists ].
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenFluidClassDefinitionRegistersIt [
	self checkClassDoesntExists.
	[
	self compileString:
'Object << #', className , '
	slots: { };
	package: ''Calypso-SystemTools-Core-Tests'''.
	self assert: self classExists.
	
	] ensure: [ self deleteClassIfExists ].
]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenMethodRegistersIt [
	| selector |
	selector := #banana.
	(context class class lookupSelector: selector) ifNotNil: [ self error: 'I might overide an important behavior' ].
	[
	self compileString:
selector , '
	^ 1'.
	self assert: (context class class methodDict includes: selector) isNotNil.
	
	] ensure: [ context class class removeSelector: selector ].

]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testAcceptAWellWrittenTraitDefinitionRegistersIt [
	self error: #todo.
	self compileString: 'Object << #MyClass
		slots: { #s #s };
    package: ''Calypso-SystemQueries-Tests'''.
	editor applyChanges .

]

{ #category : #test }
ClyClassDefinitionEditorToolMorphTest >> testRedefineAClassShouldRedefineIt [
	|  newClass oldUIManager |
	self checkClassDoesntExists.

	self compileString: 
'Object << #' , className , '
	package: ''Calypso-SystemQueries-Tests'''.
	
	oldUIManager := UIManager default.
	[
		UIManager default: NonInteractiveUIManager new.
		[ 
			self compileString: 
'Object << #' , className , '
	slots: {#s};
	package: ''Calypso-SystemQueries-Tests'''.
		] 
			on: ProvideAnswerNotification 
			do:[ :e | e resume: true ]
	]
	ensure: [ UIManager default: oldUIManager ].

	newClass := self class environment classNamed: #MyClass.
	[ self assert: newClass allSlots size equals: 1 ] 
		ensure: [ self deleteClassIfExists ]
]
