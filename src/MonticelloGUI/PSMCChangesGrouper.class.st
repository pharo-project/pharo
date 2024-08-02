"
I am a visitor on MCDefinition objects. I will group a collection of definitions into a tree that will be shown in the PSMCPatchMorph.
"
Class {
	#name : 'PSMCChangesGrouper',
	#superclass : 'Object',
	#instVars : [
		'nonClassChanges',
		'classes',
		'model',
		'currentOperation',
		'classChanges'
	],
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'helper methods' }
PSMCChangesGrouper class >> group: aCollectionOfPatches [

	^ self new
		model: aCollectionOfPatches;
		group
]

{ #category : 'grouping' }
PSMCChangesGrouper >> ensureClassChangeWrapperNamed: aClassName [

	(classes includes: aClassName)
		ifFalse: [ classes add: aClassName.
				classChanges add: (PSMCClassChangeWrapper with: aClassName model: model ) ]
]

{ #category : 'grouping' }
PSMCChangesGrouper >> group [

	model do: [:operation | self visit: operation ].
	^ (nonClassChanges sorted: [:a :b | a item <= b item]),
		(classChanges sorted: [:a :b | a item <= b item])
]

{ #category : 'initialization' }
PSMCChangesGrouper >> initialize [

	super initialize.
	classChanges := OrderedCollection new.
	nonClassChanges := OrderedCollection new.
	classes := Set new
]

{ #category : 'grouping' }
PSMCChangesGrouper >> model: aCollection [

	model := aCollection
]

{ #category : 'grouping' }
PSMCChangesGrouper >> visit: aMCOperation [

	| previous |
	previous := currentOperation.
	currentOperation := aMCOperation.
	currentOperation definition accept: self.
	currentOperation := previous
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitClassDefinition: aMCClassDefinition [

	self ensureClassChangeWrapperNamed: aMCClassDefinition fullClassName
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitClassTraitDefinition: aMCClassTraitDefinition [

	"Do nothing for class traits (similar to #visitMetaclassDefinition:)"
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitDefaultDefinition: aDefinition [

	nonClassChanges add: aDefinition
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitMetaclassDefinition: aMCClassDefinition [

	"Do nothing for metaclasses"
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitMethodDefinition: aMCMethodDefinition [

	self ensureClassChangeWrapperNamed: aMCMethodDefinition fullClassName
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitOrganizationDefinition: aMCOrganizationDefinition [

	nonClassChanges add: (currentOperation patchWrapper model: model)
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitScriptDefinition: aMCScriptDefinition [

	"do nothing for script definitions"
]

{ #category : 'visitor' }
PSMCChangesGrouper >> visitTraitDefinition: aMCTraitDefinition [

	self ensureClassChangeWrapperNamed: aMCTraitDefinition fullClassName
]
