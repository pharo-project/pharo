"
I am a transformation for creating accessors for variables.
- Setters and getters are only created if they do not already exist in the class and its subclasses.

I'm a transformation because 
- I do not modify existing direct variable accesses.  
- I do not check that method with the same name are shadowed in the superclass.

I handle correctly classVariable (sharedVariable) as well as instance variables of the metaclass. In both cases, the setter and getters are defined on the class side.

I am used by a couple of other refactorings creating new variables and accessors.

My precondition is that the variable name is defined for this class.

### Implementation points

In case the variable of a setter/getter is used in a sequence of assignments, e.g., y := x := 2, the generated code for the setter will generate
	
```	
x: anObject

	^ x := anObject
```	

if the class does not contain such sequence of assignements, the setter will be 

```	
x: anObject
	x := anObject
```

"
Class {
	#name : 'ReCreateAccessorsForVariableTransformation',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'classVariable',
		'needsReturn',
		'selector',
		'getterMethodName',
		'setterMethodName'
	],
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'displaying' }
ReCreateAccessorsForVariableTransformation class >> basicMenuItemString [

	^ 'Generate accessors'
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> classVariable: aVarName class: aClass [
	^ self variable: aVarName class: aClass classVariable: true
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> instanceVariable: aVarName class: aClass [
	^ self variable: aVarName class: aClass classVariable: false
]

{ #category : 'accessing' }
ReCreateAccessorsForVariableTransformation class >> isTransformation [

	^ true
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> model: aRBSmalltalk classVariable: aVarName class: aClass [

	^ self model: aRBSmalltalk variable: aVarName class: aClass classVariable: true
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> model: aRBSmalltalk instanceVariable: aVarName class: aClass [

	^ self model: aRBSmalltalk variable: aVarName class: aClass classVariable: false
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean [
	^(self
		model: aRBSmalltalk
		variable: aVarName
		class: aClass)
		classVariable: aBoolean;
		yourself
]

{ #category : 'instance creation' }
ReCreateAccessorsForVariableTransformation class >> variable: aVarName class: aClass classVariable: aBoolean [
	^(self variable: aVarName class: aClass)
		classVariable: aBoolean; yourself
]

{ #category : 'preconditions' }
ReCreateAccessorsForVariableTransformation >> applicabilityPreconditions [

	^ { (classVariable
		   ifTrue: [
				RBCondition definesClassVariable: variableName asSymbol in: class ]
		   ifFalse: [
				RBCondition definesInstanceVariable: variableName in: class ]) }
]

{ #category : 'initialization' }
ReCreateAccessorsForVariableTransformation >> classVariable: aBoolean [
	classVariable := aBoolean
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> createGetterAccessor [
	"Only define a getter if it is not already defined in the class or its subclasses."

	getterMethodName := self findGetterMethod.
	getterMethodName ifNil: [ getterMethodName := self defineGetterMethod ]
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> createMatcher [
	^ self parseTreeSearcherClass getterMethod: variableName
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> createSetterAccessor [

	setterMethodName := self findSetterMethod.
	setterMethodName ifNil: [ setterMethodName := self defineSetterMethod ]
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> defineGetterMethod [

	self generateChangesFor: 
		(RBAddMethodTransformation
			 sourceCode: ('<1s><r><r><t>^ <2s>'
					  expandMacrosWith: self selector
					  with: variableName)
			 in: self definingClass
			 withProtocol: #accessing ).

	^ selector
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> defineSetterMethod [
	| definingClass string |
	definingClass := self definingClass.
	string := self needsReturnForSetter
				ifTrue: ['<1s> anObject<r><r><t>^ <2s> := anObject']
				ifFalse: ['<1s> anObject<r><r><t><2s> := anObject'].
	selector := self
						safeMethodNameFor: definingClass
						basedOn: variableName asString , ':'.
						
	self generateChangesFor: 
		(RBAddMethodTransformation
		 	 sourceCode: (string expandMacrosWith: selector with: variableName)
			 in: definingClass
		 	 withProtocol: #accessing ).

	^selector
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> definingClass [
	^ classVariable
		ifTrue: [ class classSide ]
		ifFalse: [ class ]
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> findGetterMethod [
	"Look for possible already existing getter method (a method accessing the instance variable in a class or its subclasses).
	This information will be used to avoid creating a new getter."

	| definingClass matcher |
	definingClass := self definingClass.
	matcher := self createMatcher.
	^ self possibleGetterSelectors
		detect: [ :each |
			(self checkClass: definingClass selector: each using: matcher) isNotNil
				and: [ (definingClass subclassRedefines: each) not ] ]
		ifNone: [ nil ]
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> findSetterMethod [
	"Look for possible already existing setter method (a method accessing the instance variable in a class or its subclasses).
	This information will be used to avoid creating a new setter."

	| definingClass matcher |
	definingClass := self definingClass.
	matcher := self needsReturnForSetter
		ifTrue: [ self parseTreeSearcherClass returnSetterMethod: variableName ]
		ifFalse: [ self parseTreeSearcherClass setterMethod: variableName ].
	^ self possibleSetterSelectors
		detect: [ :each |
			(self checkClass: definingClass selector: each using: matcher) isNotNil
				and: [ (definingClass subclassRedefines: each) not ] ]
		ifNone: [ nil ]
]

{ #category : 'client refactoring API' }
ReCreateAccessorsForVariableTransformation >> getterMethod [

	self deprecated: 'Use getterMethodName' transformWith: '`@arg getterMethod' -> '`@arg getterMethodName'.
	^ self getterMethodName
]

{ #category : 'client refactoring API' }
ReCreateAccessorsForVariableTransformation >> getterMethodName [

	^getterMethodName
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> methodsReferencingVariable [

	^ classVariable
		ifTrue: [ self definingClass whichSelectorsReferToClassVariable: variableName ]
		ifFalse: [ self definingClass whichSelectorsReferToInstanceVariable: variableName ]
]

{ #category : 'testing' }
ReCreateAccessorsForVariableTransformation >> needsReturnForSetter [
	"In case the variable is used in a sequence of assignments y := x := 2, the generated code for the setter will generate

	x: anObject

			^ x := anObject

	if the class does not contain such sequence of assignements, the setter will be

	x: anObject

			x := anObject
	"



	needsReturn
		ifNil:
			[ needsReturn := self usesAssignmentOf: variableName in: class classVariable: classVariable ].
	^ needsReturn
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> possibleGetterSelectors [
	^self methodsReferencingVariable select: [:each | each numArgs == 0]
]

{ #category : 'private - accessing' }
ReCreateAccessorsForVariableTransformation >> possibleSetterSelectors [
	^self methodsReferencingVariable select: [:each | each numArgs == 1]
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> privateTransform [
	self
		createGetterAccessor;
		createSetterAccessor
]

{ #category : 'transforming' }
ReCreateAccessorsForVariableTransformation >> selector [
	^ selector ifNil: [
		selector := self safeMethodNameFor: self definingClass
				basedOn: variableName asString. ]
]

{ #category : 'client refactoring API' }
ReCreateAccessorsForVariableTransformation >> setterMethod [
	self deprecated: 'Use getterMethodName' transformWith: '`@arg setterMethod' -> '`@arg setterMethodName'.

	^ self setterMethodName
]

{ #category : 'client refactoring API' }
ReCreateAccessorsForVariableTransformation >> setterMethodName [

	^ setterMethodName
]

{ #category : 'storing' }
ReCreateAccessorsForVariableTransformation >> storeOn: aStream [
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

{ #category : 'testing' }
ReCreateAccessorsForVariableTransformation >> usesAssignmentOf: aString in: aClass classVariable: isClassVar [
	"Returns whether the variable aString is used in a assignement sequence e.g., x := y := 2 in the class and its subclasses."

	| matcher definingClass |
	matcher := self parseTreeSearcher.
	matcher
		answer: false;
		matches: aString , ' := ``@object'
			do: [ :aNode :answer |
			answer
				or: [ aNode isUsedAsReturnValue and: [ aNode methodNode selector ~= aString ] ] ].
	definingClass := isClassVar
		ifTrue: [ aClass instanceSide ]
		ifFalse: [ aClass ].
	^ (definingClass withAllSubclasses
		,
			(isClassVar
				ifTrue: [ definingClass classSide withAllSubclasses ]
				ifFalse: [ #() ])
		anySatisfy: [ :each |
			((isClassVar
				ifTrue: [ each whichSelectorsReferToClassVariable: aString ]
				ifFalse: [ each whichSelectorsReferToInstanceVariable: aString ])
				anySatisfy:
					[ :sel | self checkClass: each selector: sel using: matcher ]) ])
]
