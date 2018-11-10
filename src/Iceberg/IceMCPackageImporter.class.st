"
I am an importer of MCPackages into an IceNode. I am created from a version and a node. I calculate the snapshot of that definition and import all those definitions into my node, delegating to a IceMCDefinitionImporter.
"
Class {
	#name : #IceMCPackageImporter,
	#superclass : #Object,
	#instVars : [
		'packageName',
		'version',
		'package'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #importing }
IceMCPackageImporter >> importOn: aNode [
	
	| packageNode snapshot definition |
	
	"If the version does not include the package, we do not calculate a snapshot nor import it"
	(version includesPackageNamed: self packageName)
		ifFalse: [ ^ self ].
	
	definition := IcePackageDefinition named: self packageName.
	packageNode := aNode addChild: definition.
	definition path: packageNode path.
	
	snapshot := self snapshot.
	snapshot definitions do: [ :e | 
		e accept: (IceMCDefinitionImporter new
			packageNode: packageNode;
			snapshot: snapshot;
			yourself) ].
	^ packageNode
]

{ #category : #accessing }
IceMCPackageImporter >> package [
	^ package
]

{ #category : #accessing }
IceMCPackageImporter >> package: aPackage [
	
	package := aPackage
]

{ #category : #accessing }
IceMCPackageImporter >> packageName [
	
	^ self package name
]

{ #category : #accessing }
IceMCPackageImporter >> snapshot [
	
	^ version snapshotForPackage: self package
]

{ #category : #accessing }
IceMCPackageImporter >> version: aVersion [
	
	version := aVersion
]
