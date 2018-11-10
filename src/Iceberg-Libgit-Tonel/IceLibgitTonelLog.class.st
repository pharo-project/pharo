"
I'm a tool to ask the full history of a method (based on tonel file format).
"
Class {
	#name : #IceLibgitTonelLog,
	#superclass : #IceLog,
	#category : #'Iceberg-Libgit-Tonel'
}

{ #category : #testing }
IceLibgitTonelLog class >> isAvailableFor: aRepository [
	^ aRepository writerClass id = #tonel
]

{ #category : #private }
IceLibgitTonelLog >> basepathOf: aPath [ 
	^ aPath copyAfterLast: $/
]

{ #category : #private }
IceLibgitTonelLog >> definitionFor: aMethod path: aPath commit: aCommit [
	^ ((IceLibgitTonelLogReader fileName: aPath on: aCommit)
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
IceLibgitTonelLog >> fileNameForMethod: aMethod [
	| path |
	
	path := OrderedCollection new.
	self repository subdirectory 
		ifNotEmpty: [ :subDir | path add: subDir ].
	path add: (self tonelPackageName: aMethod package).
	path add: (self tonelMethodClassOrTraitName: aMethod).
	
	^ String streamContents: [ :stream |
		path asStringOn: stream delimiter: '/' ]
]

{ #category : #private }
IceLibgitTonelLog >> toFileName: aMethod [
	^ self fileNameForMethod: aMethod
]

{ #category : #private }
IceLibgitTonelLog >> tonelClassExtension: aClass [
	aClass isTrait ifTrue: [ ^ '.trait' ].
	^ '.class'
]

{ #category : #private }
IceLibgitTonelLog >> tonelClassOrTraitName: aClass [
	^ aClass name, (self tonelClassExtension: aClass), '.st'
]

{ #category : #private }
IceLibgitTonelLog >> tonelMethodClassExtension: aMethod [
	aMethod isExtension ifTrue: [ ^ '.extension' ].
	aMethod origin isTrait ifTrue: [ ^ '.trait' ].
	^ '.class'
	
]

{ #category : #private }
IceLibgitTonelLog >> tonelMethodClassOrTraitName: aMethod [
	^ aMethod origin instanceSide name, (self tonelMethodClassExtension: aMethod), '.st'
	
	
]

{ #category : #private }
IceLibgitTonelLog >> tonelPackageName: aPackage [
	^ aPackage name	
]
