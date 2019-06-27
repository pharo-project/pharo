"
I represent variables of class with explicit information about defining class.
I am always created on some actual variable instance like Slot or ClassVariable:

	ClyVariable on: aSlot definedIn: aClass

My subclasses implement three methods:
- isAccessibleFrom: aClass
- isReadIn: aMethod
- isWrittenIn: aMethod

Internal Representation and Key Implementation Points.

    Instance Variables
	actualVariable:		<Slot, ClassVariable>
	declaringClass:		<Class>
"
Class {
	#name : #ClyVariable,
	#superclass : #Object,
	#instVars : [
		'actualVariable',
		'definingClass'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'browser item preparation' }
ClyVariable class >> convertToCalypsoBrowserItem: myItem [

	^ClyBrowserItem named: myItem name with: myItem
]

{ #category : #sorting }
ClyVariable class >> defaultSortFunctionForCalypso [
	^ClySortByNameFunction ascending
]

{ #category : #sorting }
ClyVariable class >> itemsSortOrderForCalypso [ 
	^1.2
]

{ #category : #'instance creation' }
ClyVariable class >> named: aSymbol definedIn: aClass [
	self subclassResponsibility 
]

{ #category : #'instance creation' }
ClyVariable class >> on: aVariableOrSlot definedIn: aClass [
	^self new 
		actualVariable: aVariableOrSlot;
		definingClass: aClass
]

{ #category : #'instance creation' }
ClyVariable class >> on: aVariableOrSlot visibleFrom: aClass [
	^self new 
		actualVariable: aVariableOrSlot;
		detectDefiningClassFrom: aClass
]

{ #category : #queries }
ClyVariable class >> queryClass [
	^ClyAllVariablesQuery
]

{ #category : #queries }
ClyVariable class >> shortName [
	^'Vars'
]

{ #category : #comparing }
ClyVariable >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ definingClass = anObject definingClass
		and: [ actualVariable = anObject actualVariable ]
]

{ #category : #accessing }
ClyVariable >> actualVariable [
	^ actualVariable
]

{ #category : #accessing }
ClyVariable >> actualVariable: anObject [
	actualVariable := anObject
]

{ #category : #accessing }
ClyVariable >> definingClass [
	^ definingClass
]

{ #category : #accessing }
ClyVariable >> definingClass: anObject [
	definingClass := anObject
]

{ #category : #accessing }
ClyVariable >> detectDefiningClassFrom: anUserClass [

	self subclassResponsibility 
]

{ #category : #comparing }
ClyVariable >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ definingClass hash bitXor: actualVariable hash
]

{ #category : #testing }
ClyVariable >> isAccessibleFrom: aClass [
	self subclassResponsibility 
]

{ #category : #testing }
ClyVariable >> isClassVariable [
	self subclassResponsibility 
]

{ #category : #testing }
ClyVariable >> isLive [

	^false
]

{ #category : #testing }
ClyVariable >> isReadIn: aMethod [
	
	(self isAccessibleFrom: aMethod origin)	ifFalse: [ ^false ].
	
	^actualVariable isReadIn: aMethod 
]

{ #category : #testing }
ClyVariable >> isReferencedIn: aMethod [
	
	(self isAccessibleFrom: aMethod origin)	ifFalse: [ ^false ].
		
	^(actualVariable isReadIn: aMethod) or: [ actualVariable isWrittenIn: aMethod ]
]

{ #category : #testing }
ClyVariable >> isWrittenIn: aMethod [
	
	(self isAccessibleFrom: aMethod origin)	ifFalse: [ ^false ].
		
	^actualVariable isWrittenIn: aMethod 
]

{ #category : #accessing }
ClyVariable >> name [
	^actualVariable name
]

{ #category : #operations }
ClyVariable >> openBrowser [

	definingClass browse
]

{ #category : #operations }
ClyVariable >> openInspector [

	actualVariable inspect
]

{ #category : #printing }
ClyVariable >> printAsConstantQueryItem [
	^self name
]

{ #category : #printing }
ClyVariable >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPut: $(;
		nextPutAll: self name;
		nextPut: $)
]
