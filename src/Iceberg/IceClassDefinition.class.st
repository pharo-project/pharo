"
I represent a class read from an Iceberg repository, either from a commit or from the working directory in disk.

My contents is the class definition code.

See my superclasses for more information.
"
Class {
	#name : #IceClassDefinition,
	#superclass : #IceMethodContainerDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceClassDefinition >> accept: aVisitor [ 
	
	^ aVisitor visitClassDefinition: self
]

{ #category : #patching }
IceClassDefinition >> addModification: anIceModification toPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses if their mcDefinition is nil.
	They should be added automatically when added the instance side."
	(self isMeta and: [ mcDefinition isNil ])
		ifTrue: [ ^ self ].
	
	aMCPatcher
		modifyDefinition: anIceModification rightDefinition asMCDefinition
		to: anIceModification leftDefinition asMCDefinition
]

{ #category : #patching }
IceClassDefinition >> addToPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses if their mcDefinition is nil.
	They should be added automatically when added the instance side."
	(self isMeta and: [ mcDefinition isNil ])
		ifTrue: [ ^ self ].
	
	"We should only be in this case if there is not instace-side changes."
	aMCPatcher addDefinition: self asMCDefinition
]

{ #category : #accessing }
IceClassDefinition >> contents [
	mcDefinition ifNil: [ ^ '' ].
	^ self isMeta
		ifFalse: [ mcDefinition fullDefinitionString ]
		ifTrue: [ mcDefinition classDefinitionString ]
]

{ #category : #testing }
IceClassDefinition >> isClassDefinition [
	
	^ true
]

{ #category : #patching }
IceClassDefinition >> removeFromPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses. They will be removed automatically if the instance side is removed"
	isMeta ifTrue: [ ^ true ].
	
	aMCPatcher removeDefinition: self asMCDefinition 
]
