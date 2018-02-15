"
I am a refactoring for creating accessors for variables.

I am used by a couple of other refactorings  creating new variables and accessors.

My procondition is that the variable name is defined for this class.
"
Class {
	#name : #RBCreateAccessorsForVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#instVars : [
		'getterMethod',
		'setterMethod',
		'classVariable',
		'needsReturn'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBCreateAccessorsForVariableRefactoring class >> model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean [ 
	^(self 
		model: aRBSmalltalk
		variable: aVarName
		class: aClass)
		classVariable: aBoolean;
		yourself
]

{ #category : #'instance creation' }
RBCreateAccessorsForVariableRefactoring class >> variable: aVarName class: aClass classVariable: aBoolean [ 
	^(self variable: aVarName class: aClass)
		classVariable: aBoolean; yourself
]

{ #category : #'initialize-release' }
RBCreateAccessorsForVariableRefactoring >> classVariable: aBoolean [ 
	classVariable := aBoolean
]

{ #category : #transforming }
RBCreateAccessorsForVariableRefactoring >> createGetterAccessor [
	getterMethod := self findGetterMethod.
	getterMethod isNil ifTrue: [getterMethod := self defineGetterMethod]
]

{ #category : #transforming }
RBCreateAccessorsForVariableRefactoring >> createSetterAccessor [
	setterMethod := self findSetterMethod.
	setterMethod isNil ifTrue: [setterMethod := self defineSetterMethod]
]

{ #category : #transforming }
RBCreateAccessorsForVariableRefactoring >> defineGetterMethod [
	| selector definingClass |
	definingClass := self definingClass.
	selector := self safeMethodNameFor: definingClass
				basedOn: variableName asString.
	definingClass 
		compile: ('<1s><r><t>^ <2s>' expandMacrosWith: selector with: variableName)
		classified: #(#accessing).
	^selector
]

{ #category : #transforming }
RBCreateAccessorsForVariableRefactoring >> defineSetterMethod [
	| selector definingClass string |
	definingClass := self definingClass.
	string := self needsReturnForSetter 
				ifTrue: ['<1s> anObject<r><t>^ <2s> := anObject']
				ifFalse: ['<1s> anObject<r><t><2s> := anObject'].
	selector := self safeMethodNameFor: definingClass
				basedOn: variableName asString , ':'.
	definingClass 
		compile: (string expandMacrosWith: selector with: variableName)
		classified: #accessing.
	^selector
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> definingClass [
	^ classVariable 
		ifTrue: [ class classSide ]
		ifFalse: [ class ]
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> findGetterMethod [
	| definingClass matcher |
	definingClass := self definingClass.
	matcher := RBParseTreeSearcher getterMethod: variableName.
	^self possibleGetterSelectors detect: 
			[:each | 
			(self 
				checkClass: definingClass
				selector: each
				using: matcher) notNil 
				and: [(definingClass subclassRedefines: each) not]]
		ifNone: [nil]
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> findSetterMethod [
	| definingClass matcher |
	definingClass := self definingClass.
	matcher := self needsReturnForSetter 
				ifTrue: [RBParseTreeSearcher returnSetterMethod: variableName]
				ifFalse: [RBParseTreeSearcher setterMethod: variableName].
	^self possibleSetterSelectors detect: 
			[:each | 
			(self 
				checkClass: definingClass
				selector: each
				using: matcher) notNil 
				and: [(definingClass subclassRedefines: each) not]]
		ifNone: [nil]
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> getterMethod [
	^getterMethod
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> methodsReferencingVariable [
	^classVariable 
		ifTrue: [self definingClass whichSelectorsReferToClassVariable: variableName]
		ifFalse: 
			[self definingClass whichSelectorsReferToInstanceVariable: variableName]
]

{ #category : #testing }
RBCreateAccessorsForVariableRefactoring >> needsReturnForSetter [
	needsReturn isNil 
		ifTrue: 
			[needsReturn := self 
						usesAssignmentOf: variableName
						in: class
						classVariable: classVariable].
	^needsReturn
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> possibleGetterSelectors [
	^self methodsReferencingVariable select: [:each | each numArgs == 0]
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> possibleSetterSelectors [
	^self methodsReferencingVariable select: [:each | each numArgs == 1]
]

{ #category : #preconditions }
RBCreateAccessorsForVariableRefactoring >> preconditions [
	^classVariable 
		ifTrue: [RBCondition definesClassVariable: variableName asSymbol in: class]
		ifFalse: [RBCondition definesInstanceVariable: variableName in: class]
]

{ #category : #'private-accessing' }
RBCreateAccessorsForVariableRefactoring >> setterMethod [
	^setterMethod
]

{ #category : #printing }
RBCreateAccessorsForVariableRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	classVariable storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBCreateAccessorsForVariableRefactoring >> transform [
	self
		createGetterAccessor;
		createSetterAccessor
]

{ #category : #testing }
RBCreateAccessorsForVariableRefactoring >> usesAssignmentOf: aString in: aClass classVariable: isClassVar [ 
	| matcher definingClass |
	matcher := RBParseTreeSearcher new.
	matcher
		answer: false;
		matches: aString , ' := ``@object'
			do: [ :aNode :answer | answer or: [ aNode isUsed and: [ aNode methodNode selector ~= aString] ] ].
	definingClass := isClassVar 
		ifTrue: [ aClass theNonMetaClass ]
		ifFalse: [ aClass ].
	^ (definingClass withAllSubclasses , (isClassVar 
			ifTrue: [ definingClass classSide withAllSubclasses ]
			ifFalse: 
				[ #() ]) 
		detect: 
			[ :each | 
			((isClassVar 
				ifTrue: [ each whichSelectorsReferToClassVariable: aString ]
				ifFalse: [ each whichSelectorsReferToInstanceVariable: aString ]) 
				detect: 
					[ :sel | 
					self 
						checkClass: each
						selector: sel
						using: matcher ]
				ifNone: [ nil ]) notNil ]
		ifNone: [ nil ]) notNil
]
