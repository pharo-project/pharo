"
I represent a trait read from an Iceberg repository, either from a commit or from the working directory in disk.

My contents is the trait definition.

See my superclasses for more information.
"
Class {
	#name : #IceTraitDefinition,
	#superclass : #IceMethodContainerDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceTraitDefinition >> accept: aVisitor [
	
	^ aVisitor visitTraitDefinition: self
]

{ #category : #patching }
IceTraitDefinition >> addModification: anIceModification toPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses if their mcDefinition is nil.
	They should be added automatically when added the instance side."
	(self isMeta and: [ mcDefinition isNil ])
		ifTrue: [ ^ self ].
	
	aMCPatcher
		modifyDefinition: anIceModification rightDefinition asMCDefinition
		to: anIceModification leftDefinition asMCDefinition
]

{ #category : #patching }
IceTraitDefinition >> addToPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses if their mcDefinition is nil.
	They should be added automatically when added the instance side."
	(self isMeta and: [ mcDefinition isNil ])
		ifTrue: [ ^ self ].

	"We should only be in this case if there is not instace-side changes."
	aMCPatcher addDefinition: self asMCDefinition
]

{ #category : #testing }
IceTraitDefinition >> isTraitDefinition [

	^ true
]

{ #category : #patching }
IceTraitDefinition >> removeFromPatcher: aMCPatcher [ 
	
	"We should not handle metaclasses. They will be added automatically when added the instance side"
	self isMeta ifTrue: [ ^ self ].
	
	aMCPatcher removeDefinition: self asMCDefinition
]
