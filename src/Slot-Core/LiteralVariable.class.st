"
I am abstract. Subclasses of me model meta objects for global variables (Class, Global, Pool), called ""Literal Variables"" in Smalltalk.

I am just a subclass of Association, modeling the binding of the global in either the Smalltalk globals, Undeclared or classPools.


When chaning emit* methods, do not forget to recompile exisiting code:

aGlobal usingMethods do: #recompile 

***NOTE***
When moving binding from Undeclared, we change the class of that binding to either ClassVariable or GlobalVariable.

==> when we use Global subclasses, we will either need to restrict adding variables or add a slow path where we create a new binding and update all users. But this can be done later.
"
Class {
	#name : #LiteralVariable,
	#superclass : #LiteralVariableAdoptation,
	#category : #'Slot-Core-Variables'
}

{ #category : #validating }
LiteralVariable class >> checkValidName: aString [ 
	"aString first canBeGlobalVarInitial
		ifFalse: [InvalidGlobalName signal: 'Class name does not start with a valid Global Var Initial' for: aString]"
]

{ #category : #'gt-inspector-extension' }
LiteralVariable class >> gtInspectorAllVariablesIn: composite [
 	"This provides a list of all defined Class or Global Variables"
 	<gtInspectorPresentationOrder: 20>
 	composite list 
 		title: 'All Defined Variables';
 		display: [ self allSubclasses sorted: #name ascending ];
 		tags: [ :each | { each package name } ];
 		when: [self == LiteralVariable  ].
]

{ #category : #'instance creation' }
LiteralVariable class >> named: aSymbol [ 
	self checkValidName: aSymbol.
	^ self new
		name: aSymbol;
		yourself
]

{ #category : #converting }
LiteralVariable >> asClassVariable [	
	^self
]

{ #category : #queries }
LiteralVariable >> definingClass [
	"The class defining the variable. For Globals, return nil"
	^Smalltalk globals allClasses detect: [ :class | class hasClassVariable: self ] ifNone: [ nil ]
]

{ #category : #printing }
LiteralVariable >> definitionString [
	"Every subclass that adds state must redefine either this method or #printOn:"
	^ self printString
]

{ #category : #'code generation' }
LiteralVariable >> emitStore: aMethodBuilder [
	| tempName |
	tempName := Object new.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushLiteralVariable: (AdditionalBinding key: #global value: self);
		pushTemp: tempName;
		send: #write:
]

{ #category : #'code generation' }
LiteralVariable >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteralVariable: (AdditionalBinding key: #global value: self);
		send: #read
]

{ #category : #'class building' }
LiteralVariable >> installingIn: aClass [
	"I am called by the class builder. This way a class variable can change the class it is installed in"
]

{ #category : #testing }
LiteralVariable >> isClassVariable [
	^false
]

{ #category : #testing }
LiteralVariable >> isGlobal [
	^true
]

{ #category : #testing }
LiteralVariable >> isGlobalVariable [
	^false
]

{ #category : #testing }
LiteralVariable >> isReadIn: aCompiledCode [
	^aCompiledCode readsRef: self
]

{ #category : #testing }
LiteralVariable >> isReferenced [
	"my subclasses override this if they can be referenced in code"
	^false
]

{ #category : #testing }
LiteralVariable >> isSelfEvaluating [
	^ self key isSelfEvaluating and: [self value isSelfEvaluating]
]

{ #category : #testing }
LiteralVariable >> isWrittenIn: aCompiledCode [
	^aCompiledCode writesRef: self
]

{ #category : #accessing }
LiteralVariable >> name: aString [ 
	self key: aString asSymbol
]

{ #category : #testing }
LiteralVariable >> needsFullDefinition [
	"only ClassVariable can use a simplified definition"

	^ self class ~= ClassVariable
]

{ #category : #'meta-object-protocol' }
LiteralVariable >> read [
	^self value
]

{ #category : #queries }
LiteralVariable >> usingMethods [
	^SystemNavigation new allReferencesToBinding: self
]

{ #category : #'meta-object-protocol' }
LiteralVariable >> write: anObject [
	self value: anObject.
	^anObject
]
