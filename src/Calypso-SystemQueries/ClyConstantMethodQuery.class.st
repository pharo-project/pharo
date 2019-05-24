"
I am most trivial method query which just returns constant set of methods.

I implement logic to always return live method instances:
- I filter out removed methods
- I return actual version of methods if they were modified
Look at the method #filterInstalledMethods: for details.

To create my instances use:
	ClyConstantMethodQuery with: {Rectangle >> #area}
Or with special description:
	ClyConstantMethodQuery named: 'todo methods' with: {Rectangle >> #area}
 
Internal Representation and Key Implementation Points.

    Instance Variables
	description:		<String>
	methods:		<IdentitySet of: CompiledMethod>
"
Class {
	#name : #ClyConstantMethodQuery,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'description',
		'methods'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyConstantMethodQuery class >> named: queryDescription with: methods [
	^(self with: methods)
		description: queryDescription 
]

{ #category : #'instance creation' }
ClyConstantMethodQuery class >> with: methods [
	^self new 
		methods: methods
]

{ #category : #comparing }
ClyConstantMethodQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].	
	^ methods = anObject methods
]

{ #category : #converting }
ClyConstantMethodQuery >> async [
	^self
]

{ #category : #execution }
ClyConstantMethodQuery >> buildResult: aQueryResult [

	aQueryResult fillWith: self scopedMethods
]

{ #category : #execution }
ClyConstantMethodQuery >> checkEmptyResult [

	^self installedMethods isEmpty
]

{ #category : #accessing }
ClyConstantMethodQuery >> description [
	^ description
]

{ #category : #accessing }
ClyConstantMethodQuery >> description: anObject [
	description := anObject
]

{ #category : #execution }
ClyConstantMethodQuery >> filterInstalledMethods: methodsCollection [

	^methodsCollection
		collect: [ :each | each origin localMethodNamed: each selector ifAbsent: [nil] ]
		thenReject: [ :each | each isNil or: [each package isNil or: [ each isInstalled not]]]
]

{ #category : #comparing }
ClyConstantMethodQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: methods hash
]

{ #category : #initialization }
ClyConstantMethodQuery >> initialize [
	super initialize.
	
	description := 'Methods'
]

{ #category : #execution }
ClyConstantMethodQuery >> installedMethods [

	^self filterInstalledMethods: methods
]

{ #category : #accessing }
ClyConstantMethodQuery >> methods [
	^ methods
]

{ #category : #accessing }
ClyConstantMethodQuery >> methods: anObject [
	methods := anObject asIdentitySet
]

{ #category : #execution }
ClyConstantMethodQuery >> scopedMethods [

	| installedMethods scopedMethods |
	installedMethods := self installedMethods.
	scopedMethods := IdentitySet new.
	
	scope methodsDo: [ :each | 
		(installedMethods includes: each) ifTrue: [ 
			scopedMethods add: each.
			installedMethods remove: each.
			installedMethods ifEmpty: [ ^scopedMethods ] ]].
	
	^scopedMethods
]

{ #category : #testing }
ClyConstantMethodQuery >> selectsMethod: aMethod [

	^methods anySatisfy: [ :each | 
		each calypsoEnvironmentType checkCalypsoItem: each isEqualTo: aMethod]
]

{ #category : #converting }
ClyConstantMethodQuery >> semiAsync [
	^self
]
