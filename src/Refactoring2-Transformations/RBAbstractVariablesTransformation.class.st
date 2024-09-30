Class {
	#name : #RBAbstractVariablesTransformation,
	#superclass : #RBTransformation,
	#instVars : [
		#tree,
		#fromClass,
		#instVarReaders,
		#instVarWriters,
		#classVarReaders,
		#classVarWriters,
		#toClasses,
		#ignore
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #'instance creation' }
RBAbstractVariablesTransformation class >> model: aRBModel abstractVariablesIn: aBRProgramNode from: fromBehavior toAll: behaviorCollection [
	^self
		model: aRBModel
		abstractVariablesIn: aBRProgramNode
		from: fromBehavior
		toAll: behaviorCollection
		ignoring: nil
]

{ #category : #'instance creation' }
RBAbstractVariablesTransformation class >> model: aRBSmalltalk abstractVariablesIn: aBRProgramNode from: fromBehavior toAll: behaviorCollection ignoring: aVariableName [
	^ self new
		model: aRBSmalltalk;
		abstractVariablesIn: aBRProgramNode
			from: fromBehavior
			toAll: behaviorCollection
			ignoring: aVariableName;
		yourself
]

{ #category : #executing }
RBAbstractVariablesTransformation >> abstractClassVariable: aString [
	| transformation rewriter nonMetaClass |
	nonMetaClass := fromClass instanceSide.
	transformation := (RBAddVariableAccessorTransformation
		model: self model
		variable: aString
		class: nonMetaClass
		classVariable: true)
		transform.
	rewriter := self parseTreeRewriter.
	fromClass isMeta
		ifTrue:
			[ rewriter
				replace: aString , ' := ``@object'
					with: ('self <1s> ``@object' expandMacrosWith: transformation setterMethod);
				replace: aString
					with: 'self ' , transformation getterMethod ]
		ifFalse:
			[ rewriter
				replace: aString , ' := ``@object'
					with: ('self class <1s> ``@object' expandMacrosWith: transformation setterMethod);
				replace: aString
					with: 'self class ' , transformation getterMethod ].
	(rewriter executeTree: tree) ifTrue: [ tree := rewriter tree ]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> abstractClassVariables [
	| variables |
	(classVarReaders isEmpty and: [ classVarWriters isEmpty ])
		ifTrue: [ ^ self ].
	variables := Set new.
	variables
		addAll: classVarReaders;
		addAll: classVarWriters.
	variables do: [ :each | self abstractClassVariable: each ]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> abstractInstanceVariable: aString [
	| transformation rewriter |
	transformation := (RBAddVariableAccessorTransformation
		model: self model
		variable: aString
		class: fromClass
		classVariable: false)
		transform.
	rewriter := self parseTreeRewriter.
	rewriter
		replace: aString , ' := ``@object'
			with: ('self <1s> ``@object' expandMacrosWith: transformation setterMethod);
		replace: aString with: 'self ' , transformation getterMethod.
	(rewriter executeTree: tree) ifTrue: [tree := rewriter tree]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> abstractInstanceVariables [
	| variables |
	(instVarReaders isEmpty and: [ instVarWriters isEmpty ])
		ifTrue: [ ^ self].
	variables := Set new.
	variables
		addAll: instVarReaders;
		addAll: instVarWriters.
	variables do: [ :each | self abstractInstanceVariable: each ]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> abstractVariablesIn: aBRProgramNode from: fromBehavior toAll: behaviorCollection ignoring: aVariableName [
	| poolTransformation |
	tree := aBRProgramNode.
	fromClass := self model classObjectFor: fromBehavior.
	toClasses := behaviorCollection
				collect: [:each | self model classObjectFor: each].
	ignore := aVariableName.
	poolTransformation := (RBExpandReferencedPoolsTransformation
				model: self model
				forMethod: tree
				fromClass: fromClass
				toClasses: toClasses)
				transform.
	self computeVariablesToAbstract
]

{ #category : #executing }
RBAbstractVariablesTransformation >> classVariableNames [
	| nonMetaClass |
	nonMetaClass := fromClass instanceSide.
	^ (nonMetaClass allClassVariableNames collect: [ :each | each asString ]) asSet
]

{ #category : #executing }
RBAbstractVariablesTransformation >> computeVariablesToAbstract [
	| searcher |
	instVarReaders := Set new.
	instVarWriters := Set new.
	classVarReaders := Set new.
	classVarWriters := Set new.
	searcher := self parseTreeSearcher.
	searcher
		matches: '`var := ``@anything'
			do: [ :aNode :answer | self processAssignmentNode: aNode ];
		matches: '`var'
			do: [ :aNode :answer | self processReferenceNode: aNode ].
	searcher executeTree: tree.
	self removeDefinedClassVariables
]

{ #category : #executing }
RBAbstractVariablesTransformation >> hasVariablesToAbstract [
	^ instVarReaders notEmpty or: [ instVarWriters notEmpty or: [ classVarReaders notEmpty or: [ classVarWriters notEmpty ] ] ]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> instanceVariableNames [
	^fromClass allInstanceVariableNames asSet
]

{ #category : #executing }
RBAbstractVariablesTransformation >> parseTree [
	^tree
]

{ #category : #executing }
RBAbstractVariablesTransformation >> preconditions [

	^ self emptyCondition
]

{ #category : #executing }
RBAbstractVariablesTransformation >> privateTransform [
	self hasVariablesToAbstract
		ifTrue:
			[self
				refactoringWarning: 'This method has direct variable references which<n>will need to be converted to getter/setters.<n>Proceed anyway?'
						expandMacros].
	self abstractInstanceVariables.
	self abstractClassVariables
]

{ #category : #executing }
RBAbstractVariablesTransformation >> processAssignmentNode: aNode [
	| varName |
	varName := aNode variable name.
	ignore = varName ifTrue: [^self].
	(aNode whoDefines: varName) ifNotNil: [^self].
	(self instanceVariableNames includes: varName)
		ifTrue: [instVarWriters add: varName].
	(self classVariableNames includes: varName)
		ifTrue: [classVarWriters add: varName]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> processReferenceNode: aNode [
	| varName |
	varName := aNode name.
	ignore = varName ifTrue: [^self].
	(aNode whoDefines: varName) ifNotNil: [^self].
	(self instanceVariableNames includes: varName)
		ifTrue: [instVarReaders add: varName].
	(self classVariableNames includes: varName)
		ifTrue: [classVarReaders add: varName]
]

{ #category : #executing }
RBAbstractVariablesTransformation >> removeDefinedClassVariables [
	| selectionBlock nonMetaClass |
	nonMetaClass := fromClass instanceSide.
	selectionBlock :=
	[ :varName |
	(toClasses
		detect:
			[ :each |
			(each instanceSide includesClass: (nonMetaClass whoDefinesClassVariable: varName)) not ]
		ifNone: [ nil ]) notNil ].
	classVarReaders := classVarReaders select: selectionBlock.
	classVarWriters := classVarWriters select: selectionBlock
]
