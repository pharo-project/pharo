"
I am a refactoring for extracting a set of instance variables to a new class.

You can choose which instance variables should be moved into the new class. The new class becomes an instvar of the original class and every reference to the moved variables is replaced by a accessor call.

My precondition verifies that 
 the new instance variable is a valid variable name and not yet used in this class or its hierarchy
 the name of the new class representing the set of instance variables is a valid class name

Example:
In the following class:
Object subclass: #TextKlass
	instanceVariableNames: 'text color font style'
	classVariableNames: ''
	package: 'TestKlasses'
	
the avariables color/font/style should be moved to a new ""TextAttributes""-Class.
We apply the Split Refactoring with this three variables and select a new class name TextAttributes used as variable new ""textAttributes"".
The class definition will be changed to
Object subclass: #TextKlass
	instanceVariableNames: 'text textAttributes'
	classVariableNames: ''
	package: 'TestKlasses'
	
and every reference to the old vars 
color/font/style 
will be replaced by
 textAttributes color / textAttributes style / textAttributesFont

"
Class {
	#name : #RBSplitClassRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'class',
		'instanceVariables',
		'newClassName',
		'referenceVariableName',
		'newClass'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBSplitClassRefactoring class >> class: class instanceVariables: instVars newClassName: className referenceVariableName: newVariable [ 
	^(self new)
		class: class
			instanceVariables: instVars
			newClassName: className
			referenceVariableName: newVariable;
		yourself
]

{ #category : #'instance creation' }
RBSplitClassRefactoring class >> model: aRBSmalltalk class: class instanceVariables: instVars newClassName: className referenceVariableName: newVariable [ 
	^(self new)
		model: aRBSmalltalk;
		class: class
			instanceVariables: instVars
			newClassName: className
			referenceVariableName: newVariable;
		yourself
]

{ #category : #'private-transforming' }
RBSplitClassRefactoring >> abstractReferenceTo: each [ 
	| setterMethod replacer accessorRef getterMethod |
	accessorRef := RBCreateAccessorsForVariableRefactoring 
				variable: each
				class: newClass
				classVariable: false.
	self performComponentRefactoring: accessorRef.
	getterMethod := accessorRef getterMethod.
	setterMethod := accessorRef setterMethod.
	replacer := RBParseTreeRewriter 
				variable: each
				getter: getterMethod
				setter: setterMethod
				receiver: referenceVariableName.
	self 
		convertClasses: class withAllSubclasses
		select: [:aClass | aClass whichSelectorsReferToInstanceVariable: each]
		using: replacer.
	self performComponentRefactoring: (RBRemoveInstanceVariableRefactoring 
				remove: each
				from: class)
]

{ #category : #transforming }
RBSplitClassRefactoring >> abstractVariableReferences [
	instanceVariables do: [:each | self abstractReferenceTo: each]
]

{ #category : #'private-transforming' }
RBSplitClassRefactoring >> addClass [
	self performComponentRefactoring: (RBAddClassRefactoring 
				model: self model
				addClass: newClassName
				superclass: Object
				subclasses: #()
				category: class category).
	newClass := self model classNamed: newClassName asSymbol
]

{ #category : #'private-transforming' }
RBSplitClassRefactoring >> addInstanceVariables [
	instanceVariables do: 
			[:each | 
			self performComponentRefactoring: (RBAddInstanceVariableRefactoring 
						model: self model
						variable: each
						class: newClass)]
]

{ #category : #initialization }
RBSplitClassRefactoring >> class: aClass instanceVariables: instVars newClassName: className referenceVariableName: newVariable [ 
	class := self model classFor: aClass.
	instanceVariables := instVars.
	newClassName := className.
	referenceVariableName := newVariable
]

{ #category : #transforming }
RBSplitClassRefactoring >> createNewClass [
	self
		addClass;
		addInstanceVariables
]

{ #category : #transforming }
RBSplitClassRefactoring >> createReference [
	self performComponentRefactoring: (RBAddInstanceVariableRefactoring 
				variable: referenceVariableName
				class: class)
]

{ #category : #preconditions }
RBSplitClassRefactoring >> preconditions [
	^(RBCondition isValidClassName: newClassName) 
		& (RBCondition isGlobal: newClassName in: self model) not 
			& (RBCondition isValidInstanceVariableName: referenceVariableName for: class) 
			& (RBCondition hierarchyOf: class definesVariable: referenceVariableName) 
					not 
			& (RBCondition isGlobal: referenceVariableName in: self model) not 
			& (RBCondition definesTemporaryVariable: referenceVariableName in: class) 
					not
]

{ #category : #printing }
RBSplitClassRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' instanceVariables: '.
	instanceVariables asArray storeOn: aStream.
	aStream
		nextPutAll: ' newClassName: #';
		nextPutAll: newClassName;
		nextPutAll: ' referenceVariableName: ''';
		nextPutAll: referenceVariableName;
		nextPutAll: ''')'
]

{ #category : #transforming }
RBSplitClassRefactoring >> transform [
	self
		createNewClass;
		createReference;
		abstractVariableReferences
]
