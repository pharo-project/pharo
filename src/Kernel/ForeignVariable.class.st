"
I model an access to variables visible from any given context. I unify an API to work with different kind of variables using single reflective API:

	foreignVariable read.
	foreignVariable write: newValue.
	
This way variables from one given context can be ""migrated"" to another enviroment:

- First create my instance:

	| temp |
	foreignVariable := ForeignVariable named: #temp fromContext: thisContext.
	
- Then use it in another expression: 

	 Smalltalk compiler
		bindings: {foreignVariable};
		evaluate: '1+temp'.

For example I am used to implement debugIt in transparent way so that debugger over expression with a temp shows it as a normal variable without any trick.

To create my instances use one of preferred messages:

	ForeignVariable named: #temp fromContext: thisContext.
	ForeignVariable fromContext: thisContext variable: aVariable.

Or use a variable message: 

	aVariable asForeignVariableFrom: aContext
 
Internal Representation and Key Implementation Points.

    Instance Variables
	actualVariable:		<Variable>
	foreignContext:		<Context>
"
Class {
	#name : #ForeignVariable,
	#superclass : #Variable,
	#instVars : [
		'foreignContext',
		'actualVariable'
	],
	#category : #'Kernel-Variables'
}

{ #category : #'instance creation' }
ForeignVariable class >> fromContext: aContext variable: aVariable [

	^self new 
		foreignContext: aContext;
		actualVariable: aVariable
]

{ #category : #'instance creation' }
ForeignVariable class >> named: aString fromContext: aContext [ 
	^self 
		fromContext: aContext 
		variable: (aContext lookupVar: aString)
]

{ #category : #accessing }
ForeignVariable >> actualVariable [
	^ actualVariable
]

{ #category : #accessing }
ForeignVariable >> actualVariable: aVariable [
	actualVariable := aVariable.
	name := actualVariable name
]

{ #category : #converting }
ForeignVariable >> asForeignVariableFrom: aContext [
	^self
]

{ #category : #'code generation' }
ForeignVariable >> emitStore: aMethodBuilder [
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

{ #category : #'code generation' }
ForeignVariable >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteral: self;
		send: #read
]

{ #category : #accessing }
ForeignVariable >> foreignContext [
	^ foreignContext
]

{ #category : #accessing }
ForeignVariable >> foreignContext: anObject [
	foreignContext := anObject
]

{ #category : #accessing }
ForeignVariable >> key [ 
	^self name
]

{ #category : #printing }
ForeignVariable >> printOn: aStream [ 
	super printOn: aStream.
	
	aStream nextPut: $(.
	actualVariable printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'meta-object-protocol' }
ForeignVariable >> read [
	^actualVariable readInContext: foreignContext 
]

{ #category : #debugging }
ForeignVariable >> readInContext: aContext [
	^self read
]

{ #category : #accessing }
ForeignVariable >> scope [ 
	^ actualVariable scope
]

{ #category : #queries }
ForeignVariable >> usingMethods [
	^actualVariable usingMethods
]

{ #category : #'meta-object-protocol' }
ForeignVariable >> write: aValue [
	^actualVariable write: aValue inContext: foreignContext 
]

{ #category : #debugging }
ForeignVariable >> write: aValue inContext: aContext [
	self write: aValue
]
