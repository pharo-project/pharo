"
I'm a tool to ask the full history of a method (based on filetree file format).
"
Class {
	#name : #IceLibgitFiletreeLog,
	#superclass : #IceLog,
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #testing }
IceLibgitFiletreeLog class >> isAvailableFor: aRepository [
	^ aRepository writerClass id = #filetree
]

{ #category : #private }
IceLibgitFiletreeLog >> basepathOf: aPath [
	^ aPath
]

{ #category : #private }
IceLibgitFiletreeLog >> cypressClassOrTraitName: aMethod [
	^ aMethod origin instanceSide name, (self cypressMethodClassExtension: aMethod)
	
]

{ #category : #private }
IceLibgitFiletreeLog >> cypressMethodClassExtension: aMethod [ 
	aMethod isExtension ifTrue: [ ^ '.extension' ].
	aMethod origin isTrait ifTrue: [ ^ '.trait' ].
	^ '.class'
]

{ #category : #private }
IceLibgitFiletreeLog >> cypressMethodName: aMethod [
	^ (MCFileTreeStCypressWriter fileNameForSelector: aMethod selector asString), '.st'
]

{ #category : #private }
IceLibgitFiletreeLog >> cypressMethodSideName: aMethod [ 
	^ aMethod origin isClassSide
		ifTrue: [ 'class' ]
		ifFalse: [ 'instance' ]
]

{ #category : #private }
IceLibgitFiletreeLog >> cypressPackageName: aPackage [ 
	^ aPackage name, '.package'
]

{ #category : #private }
IceLibgitFiletreeLog >> definitionFor: aMethod path: aPath commit: aCommit [
	^ ((IceLibgitFiletreeLogReader fileName: aPath on: aCommit)
		package: aMethod package mcPackage; 
		packageDirectory: (self repository directoryPathStringForPackage: aMethod package);
		definitions)
		detect: [ :each | each isMethodDefinition and: [ each selector = aMethod selector  ] ]
		ifNone: [ 
			 "not found, return 'NULL' object: compare will fail anyway"
			MCMethodDefinition 
				className: aMethod origin name
				selector: aMethod selector
				category: #''
				timeStamp: nil
				source: '' ]
]

{ #category : #private }
IceLibgitFiletreeLog >> fileNameForMethod: aMethod [
	| path |
	
	path := OrderedCollection new.
	self repository subdirectory 
		ifNotEmpty: [ :subDir | path add: subDir ].
	path 
		add: (self cypressPackageName: aMethod package);
		add: (self cypressClassOrTraitName: aMethod);
		add: (self cypressMethodSideName: aMethod);
		add: (self cypressMethodName: aMethod).
	
	^ String streamContents: [ :stream |
		path asStringOn: stream delimiter: '/' ]
]

{ #category : #private }
IceLibgitFiletreeLog >> toFileName: aMethod [
	^ self fileNameForMethod: aMethod
]
