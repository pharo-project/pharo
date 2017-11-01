"
I have the common behavior for storing and loading pointer objects.
"
Class {
	#name : #FLPointerObjectCluster,
	#superclass : #FLObjectCluster,
	#instVars : [
		'variablesMapping'
	],
	#category : 'Fuel-Clusters'
}

{ #category : #serialize/materialize }
FLPointerObjectCluster >> afterMaterializationStepWith: aDecoder [  

	objects do: [ :anObject | anObject fuelAfterMaterialization ]
]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> clusterMaterializeStepWith: aMaterialization [

	super clusterMaterializeStepWith: aMaterialization.
	variablesMapping := aMaterialization decoder variablesMappingFor: theClass.
]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> clusterSerializeStepWith: aSerialization [

	super clusterSerializeStepWith: aSerialization.
	variablesMapping serializeOn: aSerialization encoder.
]

{ #category : #initialize-release }
FLPointerObjectCluster >> initializeAnalyzing: aClass [

	super initializeAnalyzing: aClass.
	variablesMapping := FLVariablesMapping 
		newAnalyzing: theClass
		references: self references
]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> materializeReferencesOf: anObject with: aDecoder [

	variablesMapping materializeReferencesOf: anObject with: aDecoder
]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> materializeReferencesStepWith: aDecoder [

	objects do: [ :anObject | self materializeReferencesOf: anObject with: aDecoder ]
]

{ #category : #analyzing }
FLPointerObjectCluster >> referencesOf: anObject do: aBlock [

	variablesMapping referencesOf: anObject do: aBlock
]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> serializeReferencesOf: anObject with: anEncoder [
	
	variablesMapping serializeReferencesOf: anObject with: anEncoder

]

{ #category : #serialize/materialize }
FLPointerObjectCluster >> serializeReferencesStepWith: anEncoder [

	objects do: [ :anObject | self serializeReferencesOf: anObject with: anEncoder ]
]
