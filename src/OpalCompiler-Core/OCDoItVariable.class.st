"
I model an access to variables from DoIt scripts. Following example demonstrates an idea:

	| temp |
	temp := 100.
	doItVar := DoItVariable named: #temp fromContext: thisContext.	
	Smalltalk compiler
		bindings: {doItVar};
		evaluate: '1+temp'.

I capture a given variable and a context and compile a method nodes using unified reflective API as:

	doItVariable read.
	doItVariable write: newValue.
	
This way variables visible from one context are indirectly accessible from another environment (from doit expressions).

For example in ""debugIt expressions"" in Debugger the temp variables from original context are shown as normal variables without any tricks.

To create my instances use following messages:

	DoItVariable named: #temp fromContext: thisContext.
	DoItVariable fromContext: thisContext variable: aVariable.

Or use a variable message: 

	aVariable asDoItVariableFrom: aContext
 
Internal Representation and Key Implementation Points.

    Instance Variables
	actualVariable:		<Variable>
	doItContext:		<Context>
"
Class {
	#name : 'OCDoItVariable',
	#superclass : 'Variable',
	#instVars : [
		'actualVariable',
		'doItContext'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'instance creation' }
OCDoItVariable class >> fromContext: aContext variable: aVariable [

	^self new
		doItContext: aContext;
		actualVariable: aVariable
]

{ #category : 'instance creation' }
OCDoItVariable class >> named: aString fromContext: aContext [
	^self
		fromContext: aContext
		variable: (aContext lookupVar: aString)
]

{ #category : 'accessing' }
OCDoItVariable >> actualVariable [
	^ actualVariable
]

{ #category : 'accessing' }
OCDoItVariable >> actualVariable: aVariable [
	actualVariable := aVariable.
	name := actualVariable name
]

{ #category : 'testing' }
OCDoItVariable >> allowsShadowing [
	^true
]

{ #category : 'accessing' }
OCDoItVariable >> doItContext [
	^ doItContext
]

{ #category : 'accessing' }
OCDoItVariable >> doItContext: anObject [
	doItContext := anObject
]

{ #category : 'code generation' }
OCDoItVariable >> emitStore: aMethodBuilder [
	"generate bytecode to call the reflective write method of the Slot"
	| tempName |
	tempName := '0slotTempForStackManipulation'.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushLiteral: self;
		pushTemp: tempName;
		send: #write:
]

{ #category : 'code generation' }
OCDoItVariable >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteral: self;
		send: #read
]

{ #category : 'testing' }
OCDoItVariable >> isArgumentVariable [
	^actualVariable isArgumentVariable
]

{ #category : 'testing' }
OCDoItVariable >> isClassVariable [
	^actualVariable isClassVariable
]

{ #category : 'testing' }
OCDoItVariable >> isGlobalVariable [
	^actualVariable isGlobalVariable
]

{ #category : 'testing' }
OCDoItVariable >> isInstanceVariable [
	^actualVariable isInstanceVariable
]

{ #category : 'testing' }
OCDoItVariable >> isLiteralVariable [
	^actualVariable isLiteralVariable
]

{ #category : 'testing' }
OCDoItVariable >> isLocalVariable [
	^actualVariable isLocalVariable
]

{ #category : 'testing' }
OCDoItVariable >> isPseudoVariable [
	^actualVariable isPseudoVariable
]

{ #category : 'testing' }
OCDoItVariable >> isSelfVariable [
	^actualVariable isSelfVariable
]

{ #category : 'testing' }
OCDoItVariable >> isSuperVariable [
	^actualVariable isSuperVariable
]

{ #category : 'testing' }
OCDoItVariable >> isTempVariable [
	^actualVariable isTempVariable
]

{ #category : 'testing' }
OCDoItVariable >> isThisContextVariable [
	^actualVariable isThisContextVariable
]

{ #category : 'testing' }
OCDoItVariable >> isThisProcessVariable [
	^actualVariable isThisProcessVariable
]

{ #category : 'testing' }
OCDoItVariable >> isUndeclaredVariable [
	^actualVariable isUndeclaredVariable
]

{ #category : 'testing' }
OCDoItVariable >> isUninitialized [
	^actualVariable isUninitialized
]

{ #category : 'testing' }
OCDoItVariable >> isWorkspaceVariable [
	^actualVariable isWorkspaceVariable
]

{ #category : 'accessing' }
OCDoItVariable >> key [
	^self name
]

{ #category : 'printing' }
OCDoItVariable >> printOn: aStream [
	super printOn: aStream.

	aStream nextPut: $(.
	actualVariable printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'meta-object-protocol' }
OCDoItVariable >> read [
	^actualVariable readInContext: doItContext
]

{ #category : 'debugging' }
OCDoItVariable >> readInContext: aContext [
	^self read
]

{ #category : 'accessing' }
OCDoItVariable >> scope [
	^ actualVariable scope
]

{ #category : 'queries' }
OCDoItVariable >> usingMethods [
	^actualVariable usingMethods
]

{ #category : 'meta-object-protocol' }
OCDoItVariable >> write: aValue [
	^actualVariable write: aValue inContext: doItContext
]

{ #category : 'debugging' }
OCDoItVariable >> write: aValue inContext: aContext [
	self write: aValue
]
