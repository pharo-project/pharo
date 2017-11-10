"
I'm an abstract class that represent an single delta element between two packages: removal, addition or modification.
"
Class {
	#name : #MCPatchOperation,
	#superclass : #Object,
	#instVars : [
		'isApplicable',
		'isConflict'
	],
	#category : #Monticello-Patching
}

{ #category : #comparing }
MCPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : #'applying operation' }
MCPatchOperation >> applyTo: anObject [

	(self isConflict and: [self isResolved not] ) ifTrue: [
		self error: 'Cannot continue until this conflict has been resolved' ].
	
	self remoteChosen ifTrue: [ self basicApplyTo: anObject ].
]

{ #category : #accessing }
MCPatchOperation >> beConflict [
	isConflict := true.
	isApplicable := nil. "not resolved"
]

{ #category : #accessing }
MCPatchOperation >> beNonConflict [
	isConflict := false.
	isApplicable := true.
]

{ #category : #accessing }
MCPatchOperation >> chooseLocal [
	isApplicable := false
]

{ #category : #accessing }
MCPatchOperation >> chooseNewer [
	self isLocalNewer 
		ifTrue: [ self chooseLocal ]
		ifFalse: [ self isRemoteNewer ifTrue: [ self chooseRemote ]]
]

{ #category : #accessing }
MCPatchOperation >> chooseOlder [
	self isRemoteNewer 
		ifTrue: [ self chooseLocal ]
		ifFalse: [ self isLocalNewer ifTrue: [ self chooseRemote ]]
]

{ #category : #accessing }
MCPatchOperation >> chooseRemote [
	isApplicable := true
]

{ #category : #accessing }
MCPatchOperation >> clearChoice [
	isApplicable := nil
]

{ #category : #accessing }
MCPatchOperation >> definition [
	^ self subclassResponsibility 
]

{ #category : #initialization }
MCPatchOperation >> initialize [
	super initialize.
	self beNonConflict
]

{ #category : #accessing }
MCPatchOperation >> inverse [
	self subclassResponsibility
]

{ #category : #testing }
MCPatchOperation >> isAddition [
	^ false
]

{ #category : #testing }
MCPatchOperation >> isClassPatch [
	^false
]

{ #category : #testing }
MCPatchOperation >> isConflict [
	^ isConflict == true
]

{ #category : #testing }
MCPatchOperation >> isLocalNewer [
	^ self localDefinition fullTimeStamp > self remoteDefinition fullTimeStamp
]

{ #category : #testing }
MCPatchOperation >> isModification [
	^ false
]

{ #category : #testing }
MCPatchOperation >> isRemoteNewer [
	^ self localDefinition fullTimeStamp < self remoteDefinition fullTimeStamp
]

{ #category : #testing }
MCPatchOperation >> isRemoval [
	^ false
]

{ #category : #testing }
MCPatchOperation >> isResolved [
	^ isConflict and: [ isApplicable notNil ]
]

{ #category : #testing }
MCPatchOperation >> localChosen [
	^ isApplicable == false

]

{ #category : #testing }
MCPatchOperation >> localDefinition [
	^ self baseDefinition
]

{ #category : #accessing }
MCPatchOperation >> operation [
	"for compatibility with MCConflict interface"
	^ self
]

{ #category : #accessing }
MCPatchOperation >> prefixForOperation: aSymbol [
	aSymbol == #insert ifTrue: [^ '+'].
	aSymbol == #remove ifTrue: [^ '-'].
	^ ' '
]

{ #category : #accessing }
MCPatchOperation >> printAnnotations: requests on: aStream [
	"Add a string for an annotation pane, trying to fulfill the CodeHolder annotationRequests"

	self definition printAnnotations: requests on: aStream.
]

{ #category : #testing }
MCPatchOperation >> remoteChosen [
	^ isApplicable ~~ false
]

{ #category : #accessing }
MCPatchOperation >> remoteDefinition [
	^ self targetDefinition
]

{ #category : #accessing }
MCPatchOperation >> status [
	^ self isResolved
		ifFalse: ['']
		ifTrue: [self remoteChosen
					ifFalse: ['L']
					ifTrue: ['R']]
]

{ #category : #accessing }
MCPatchOperation >> summarySuffix [
	^ ''
]

{ #category : #accessing }
MCPatchOperation >> targetClass [
	self subclassResponsibility.
]
