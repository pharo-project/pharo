"
I'm an abstract class that represent an single delta element between two packages: removal, addition or modification.
"
Class {
	#name : 'MCPatchOperation',
	#superclass : 'Object',
	#instVars : [
		'isApplicable',
		'isConflict'
	],
	#category : 'Monticello-Patching',
	#package : 'Monticello',
	#tag : 'Patching'
}

{ #category : 'comparing' }
MCPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : 'applying operation' }
MCPatchOperation >> applyTo: anObject [

	(self isConflict and: [self isResolved not] ) ifTrue: [
		self error: 'Cannot continue until this conflict has been resolved' ].
	
	self remoteChosen ifTrue: [ self basicApplyTo: anObject ].
]

{ #category : 'testing' }
MCPatchOperation >> baseDefinition [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
MCPatchOperation >> basicApplyTo: anObject [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
MCPatchOperation >> beConflict [
	isConflict := true.
	isApplicable := nil. "not resolved"
]

{ #category : 'accessing' }
MCPatchOperation >> beNonConflict [
	isConflict := false.
	isApplicable := true.
]

{ #category : 'accessing' }
MCPatchOperation >> chooseLocal [
	isApplicable := false
]

{ #category : 'accessing' }
MCPatchOperation >> chooseRemote [
	isApplicable := true
]

{ #category : 'accessing' }
MCPatchOperation >> clearChoice [
	isApplicable := nil
]

{ #category : 'accessing' }
MCPatchOperation >> definition [
	^ self subclassResponsibility 
]

{ #category : 'accessing' }
MCPatchOperation >> fromSource [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
MCPatchOperation >> initialize [
	super initialize.
	self beNonConflict
]

{ #category : 'accessing' }
MCPatchOperation >> inverse [
	self subclassResponsibility
]

{ #category : 'testing' }
MCPatchOperation >> isAddition [
	^ false
]

{ #category : 'testing' }
MCPatchOperation >> isClassPatch [
	^false
]

{ #category : 'testing' }
MCPatchOperation >> isConflict [
	^ isConflict == true
]

{ #category : 'testing' }
MCPatchOperation >> isModification [
	^ false
]

{ #category : 'testing' }
MCPatchOperation >> isRemoval [
	^ false
]

{ #category : 'testing' }
MCPatchOperation >> isResolved [
	^ isConflict and: [ isApplicable isNotNil ]
]

{ #category : 'testing' }
MCPatchOperation >> localChosen [
	^ isApplicable == false

]

{ #category : 'accessing' }
MCPatchOperation >> operation [
	"for compatibility with MCConflict interface"
	^ self
]

{ #category : 'testing' }
MCPatchOperation >> remoteChosen [
	^ isApplicable ~~ false
]

{ #category : 'accessing' }
MCPatchOperation >> status [
	^ self isResolved
		ifFalse: ['']
		ifTrue: [self remoteChosen
					ifFalse: ['L']
					ifTrue: ['R']]
]

{ #category : 'accessing' }
MCPatchOperation >> targetClass [
	self subclassResponsibility.
]

{ #category : 'accessing' }
MCPatchOperation >> targetDefinition [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
MCPatchOperation >> toSource [
	^ self subclassResponsibility
]
