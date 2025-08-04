"
I am a refactoring for rename instance variables.

I rename the instance variable in the class definition, in all methods refering to this variable and rename the old accessors.

My precondition verifies that the new variable is valid and not yet used in the whole class hierarchy.
"
Class {
	#name : 'RBRenameInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'newName',
		'browsedEnvironment',
		'renameAccessors'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRenameInstanceVariableRefactoring class >> model: aRBNamespace rename: aVarName to: aName in: aClass [
	^ self new
		model: aRBNamespace;
		rename: aVarName
			to: aName
			in: aClass;
		yourself
]

{ #category : 'instance creation' }
RBRenameInstanceVariableRefactoring class >> model: aRBNamespace rename: aVarName to: aName in: aClass renameAccessors: aBoolean [
	^ self new
		model: aRBNamespace;
		rename: aVarName
		to: aName
		in: aClass
		renameAccessors: aBoolean;
		yourself
]

{ #category : 'instance creation' }
RBRenameInstanceVariableRefactoring class >> rename: aVarName to: aName in: aClass [
	^ self new
		rename: aVarName
		to: aName
		in: aClass
]

{ #category : 'instance creation' }
RBRenameInstanceVariableRefactoring class >> rename: aVarName to: aName in: aClass renameAccessors: aBoolean [
	^ self new
		rename: aVarName
		to: aName
		in: aClass
		renameAccessors: aBoolean
]

{ #category : 'transforming' }
RBRenameInstanceVariableRefactoring >> addNewAccessors [
	| refactoring |
	refactoring := ReCreateAccessorsForVariableTransformation
		model: self
		variable: newName asString
		class: class
		classVariable: false.
	refactoring
		createGetterAccessor;
		createSetterAccessor
]

{ #category : 'preconditions' }
RBRenameInstanceVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (ReIsValidInstanceVariableName name: newName).
		  self preconditionDirectlyDefinesVariable.
		  (ReIsVariableNotDefinedInHierarchy name: newName class: class).
		  (ReNameIsGlobalCondition new model: self model className: newName)
			  not }
]

{ #category : 'refactoring' }
RBRenameInstanceVariableRefactoring >> browsedEnvironment [
	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]
]

{ #category : 'scripting api - conditions' }
RBRenameInstanceVariableRefactoring >> checkPreconditions [ 

	self checkApplicabilityPreconditions 
]

{ #category : 'initialization' }
RBRenameInstanceVariableRefactoring >> initialize [
	super initialize.
	renameAccessors := false
]

{ #category : 'accessing' }
RBRenameInstanceVariableRefactoring >> newName [
	^ newName
]

{ #category : 'accessing' }
RBRenameInstanceVariableRefactoring >> newName: anObject [
	newName := anObject
]

{ #category : 'preconditions' }
RBRenameInstanceVariableRefactoring >> preconditionDirectlyDefinesVariable [

	^ ReDirectlyDefinesInstanceVariableCondition
			   classNamed: class name
			   inModel: self model
			   instanceVariables: { variableName }
]

{ #category : 'transforming' }
RBRenameInstanceVariableRefactoring >> privateTransform [
	renameAccessors ifTrue: [
		self removeOldAccessors
	].

	class renameInstanceVariable: variableName to: newName around: [ self renameReferences ].

	renameAccessors ifFalse: [ ^ self ].
	self addNewAccessors.
	self renameAccessorsReferences
]

{ #category : 'transforming' }
RBRenameInstanceVariableRefactoring >> removeOldAccessors [
	| oldAccessors |
	oldAccessors := (class allSelectors
		select: [ :each | each = variableName asSymbol or: [ each = (variableName , ':') asSymbol ] ]
		thenCollect: [ :each | class methodFor: each ]) asOrderedCollection.
	"If we didn't find accessors, we will not add them later and rename references"
	oldAccessors ifEmpty: [ renameAccessors := false ].
	oldAccessors do: [ :each |
		self generateChangesFor:
			(RBRemoveMethodTransformation selector: each selector from: class) ]
]

{ #category : 'initialization' }
RBRenameInstanceVariableRefactoring >> rename: aVarName to: aName in: aClass [
	self rename: aVarName to: aName in: aClass renameAccessors: false
]

{ #category : 'initialization' }
RBRenameInstanceVariableRefactoring >> rename: aVarName to: aName in: aClass renameAccessors: aBoolean [
	self variable: aVarName class: aClass.
	newName := aName.
	renameAccessors := aBoolean
]

{ #category : 'accessing' }
RBRenameInstanceVariableRefactoring >> renameAccessors [
	^ renameAccessors
]

{ #category : 'accessing' }
RBRenameInstanceVariableRefactoring >> renameAccessors: anObject [
	renameAccessors := anObject
]

{ #category : 'transforming' }
RBRenameInstanceVariableRefactoring >> renameAccessorsReferences [
	| methods senders |
	methods := (class allSelectors select: [ :each | (class methodFor: each) isNotNil ] thenCollect: [ :each | each ])
		asOrderedCollection.
	senders := (methods
		select: [ :each | (class methodFor: each) source includesSubstring: variableName asString ]
		thenCollect: [ :each | class methodFor: each ]) asOrderedCollection.
	senders
		do: [ :each | each source: (each source copyReplaceAll: variableName asString with: newName asString asTokens: false) ].
	senders
		do: [ :each |
			(each selector = newName or: [ each selector asString = (newName asString , ':') ])
				ifFalse: [ self generateChangesFor:
								(RBAddMethodTransformation
									sourceCode: each source
									in: (model classNamed: class name)
									withProtocol: each protocols) ] ]
]

{ #category : 'transforming' }
RBRenameInstanceVariableRefactoring >> renameReferences [
	| replacer classes |
	replacer := self parseTreeRewriterClass
				rename: variableName
				to: newName
				handler:
					[self
						refactoringError: ('<1s> is already defined as a method or block temporary <n> variable in this class or one of its subclasses'
								expandMacrosWith: newName)].
	classes := class withAllSubclasses, (class isTrait ifTrue: [class users asArray ] ifFalse: [{}]).
	self
		convertClasses: classes
		select: [:aClass | aClass whichSelectorsReferToInstanceVariable: variableName]
		using: replacer
]

{ #category : 'storing' }
RBRenameInstanceVariableRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' rename: ''';
		nextPutAll: variableName;
		nextPutAll: ''' to: ''';
		nextPutAll: newName;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
