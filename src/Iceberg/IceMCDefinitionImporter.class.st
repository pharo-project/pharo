"
I am an importer that visits MCDefinitions and creates IceDefinitions from them. I install those definitions into a corresponding package node.
"
Class {
	#name : #IceMCDefinitionImporter,
	#superclass : #Object,
	#instVars : [
		'packageNode',
		'snapshot'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceMCDefinitionImporter >> ensureMethodOwnerNamed: ownerName isMeta: isMeta isTrait: isTrait isExtension: isExtension [
	
	| newDefinition |	
	newDefinition := isExtension
		ifTrue: [ IceExtensionDefinition named: ownerName isMeta: isMeta ]
		ifFalse: [ 
			isTrait
				ifFalse: [ IceClassDefinition named: ownerName isMeta: isMeta ]
				ifTrue: [ IceTraitDefinition named: ownerName isMeta: isMeta ] ].
	
	^ packageNode childAt: newDefinition key ifAbsentPut: [ newDefinition ].
]

{ #category : #testing }
IceMCDefinitionImporter >> isExtension: aString [ 
	
	snapshot definitionForMethodContainerNamed: aString ifAbsent: [ ^ true ].
	^ false
]

{ #category : #testing }
IceMCDefinitionImporter >> isTrait: aString [ 
	
	| definition |
	definition := snapshot definitionForMethodContainerNamed: aString ifAbsent: [ ^ false ].
	^ definition isTraitDefinition
]

{ #category : #accessing }
IceMCDefinitionImporter >> packageNode: anIcePackageNode [ 
	packageNode := anIcePackageNode
]

{ #category : #accessing }
IceMCDefinitionImporter >> snapshot: aMCSnapshot [ 
	snapshot := aMCSnapshot
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitClassDefinition: aMCClassDefinition [ 
	
	| classDefinitionNode |
	classDefinitionNode := self
		ensureMethodOwnerNamed: aMCClassDefinition className
		isMeta: false
		isTrait: false
		isExtension: false.
	classDefinitionNode value mcDefinition: aMCClassDefinition.
	^ classDefinitionNode
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitClassTraitDefinition: aMCClassTraitDefinition [ 

	| classTraitDefinitionNode |
	classTraitDefinitionNode := self
		ensureMethodOwnerNamed: aMCClassTraitDefinition className
		isMeta: true
		isTrait: true
		isExtension: false.
	classTraitDefinitionNode value mcDefinition: aMCClassTraitDefinition.
	^ classTraitDefinitionNode
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitMetaclassDefinition: aMCClassDefinition [ 
	
	| classDefinition |
	classDefinition := self
		ensureMethodOwnerNamed: aMCClassDefinition className
		isMeta: true
		isTrait: false
		isExtension: false.
	classDefinition value mcDefinition: aMCClassDefinition.
	^ classDefinition
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitMethodDefinition: aMCMethodDefinition [ 
	
	| ownerName isMeta isTrait ownerNode |
	
	ownerName := aMCMethodDefinition className.
	isMeta := aMCMethodDefinition classIsMeta.
	isTrait := self isTrait: aMCMethodDefinition className.
	
	ownerNode := self
		ensureMethodOwnerNamed: ownerName
		isMeta: isMeta
		isTrait: isTrait
		isExtension: (self isExtension: aMCMethodDefinition className).

	^ ownerNode addChild: (IceMethodDefinition
		className: aMCMethodDefinition className
		classIsMeta: aMCMethodDefinition classIsMeta
		protocol: aMCMethodDefinition protocol
		name: aMCMethodDefinition selector
		sourceCode: aMCMethodDefinition source)
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitOrganizationDefinition: aMCOrganizationDefinition [ 
	
	packageNode value categories: aMCOrganizationDefinition categories
]

{ #category : #visiting }
IceMCDefinitionImporter >> visitTraitDefinition: aMCTraitDefinition [ 
	
	| traitDefinitionNode |
	traitDefinitionNode := self
		ensureMethodOwnerNamed: aMCTraitDefinition className
		isMeta: false
		isTrait: true
		isExtension: false.
	traitDefinitionNode value mcDefinition: aMCTraitDefinition.
	^ traitDefinitionNode
]
