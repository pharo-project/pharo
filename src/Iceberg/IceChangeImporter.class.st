"
I am a visitor that imports changes from different sources into a tree.

I receive the node where I have to work on, a version, and the I can be visited by several sources (like git or image changes). 

I work recursively: I create new instances of myself to build the children nodes of my node.
"
Class {
	#name : #IceChangeImporter,
	#superclass : #Object,
	#instVars : [
		'parentNode',
		'sourceSnapshot',
		'targetSnapshot',
		'diff',
		'operation',
		'version'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #importing }
IceChangeImporter >> addFileNamed: aString path: aRelativePath contents: aByteArray [ 

	| fileParentNode |

	fileParentNode := parentNode.
	aRelativePath segments allButLastDo: [ :each | 
		fileParentNode := (fileParentNode includesKey: each)
			ifTrue: [ fileParentNode / each ]
			ifFalse: [ fileParentNode addChild: (IceDirectoryDefinition named: each) ].
		self assert: fileParentNode value isDirectoryDefinition ].

	fileParentNode addChild: (IceFileDefinition
		named: aString
		path: aRelativePath
		contents: aByteArray).
]

{ #category : #accessing }
IceChangeImporter >> diff: anIceDiff [ 
	diff := anIceDiff
]

{ #category : #private }
IceChangeImporter >> ensureDirectoryAtPath: aPathSegments inNode: aNode [

	| firstChild |
	aPathSegments ifEmpty: [ ^ aNode ].
	firstChild := self ensureDirectoryNamed: aPathSegments first inNode: aNode.
	^ self ensureDirectoryAtPath: aPathSegments allButFirst inNode: firstChild
]

{ #category : #private }
IceChangeImporter >> ensureDirectoryNamed: aName inNode: aNode [

	aNode childAt: aName ifPresent: [ :node |
		self assert: node value isDirectoryDefinition.
		^ node ].
	^ aNode addChild: (IceDirectoryDefinition named: aName)
]

{ #category : #private }
IceChangeImporter >> ensureSourceCodeParentNodeIn: aNode [
	
	^ self
		ensureDirectoryAtPath: diff repository subdirectoryPath segments
		inNode: aNode
]

{ #category : #accessing }
IceChangeImporter >> parentNode: anIcePackageNode [ 
	parentNode := anIcePackageNode
]

{ #category : #accessing }
IceChangeImporter >> version: aVersion [ 
	
	version := aVersion
]

{ #category : #visiting }
IceChangeImporter >> visitCypressPropertiesChange: anIceCypressPropertiesChange [ 
	
	version properties addToChangeBuilder: self
]

{ #category : #visiting }
IceChangeImporter >> visitImageChange: anIceImageChange [ 

	| importer |
	importer := IceMCPackageImporter new
		package: anIceImageChange package;
		version: version;
		yourself.
	^ importer importOn: (self ensureSourceCodeParentNodeIn: parentNode)
]

{ #category : #visiting }
IceChangeImporter >> visitProjectChange: anIceProjectChange [ 
	
	"Import the project file extracted from the commit without utterly post-treatment to create the new project file"
	
	version project addToChangeBuilder: self
]
