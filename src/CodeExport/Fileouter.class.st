"
I drive the file out of packages, classes and methods.
"
Class {
	#name : 'Fileouter',
	#superclass : 'Object',
	#instVars : [
		'behaviors',
		'methods',
		'exporter',
		'packageDefinitions'
	],
	#category : 'CodeExport-Base',
	#package : 'CodeExport',
	#tag : 'Base'
}

{ #category : 'API' }
Fileouter class >> fileoutPackage: aPackage [

	^ self new
		addFullPackage: aPackage;
		fileOut
]

{ #category : 'API - configuration' }
Fileouter >> addFullClass: aClass [

	behaviors add: aClass instanceSide.
	methods addAll: aClass instanceSide localMethods.
	methods addAll: aClass classSide localMethods.	
]

{ #category : 'API - configuration' }
Fileouter >> addFullPackage: aPackage [

	packageDefinitions add: aPackage.
	behaviors addAll: aPackage definedClasses.
	methods addAll: aPackage methods.
	methods addAll: aPackage extensionMethods.	
]

{ #category : 'API - configuration' }
Fileouter >> addMethod: aMethod [

	methods add: aMethod
]

{ #category : 'accessing' }
Fileouter >> behaviors [

	^ behaviors
]

{ #category : 'accessing' }
Fileouter >> behaviors: anObject [

	behaviors := anObject
]

{ #category : 'private - writing' }
Fileouter >> fileOut [

	| sortedBehaviors sortedMethods |
	sortedBehaviors := self sortBehaviors.
	sortedMethods := self methods sorted: [ :a :b | a name < b name ].

	self writeFileoutVersion.
"	packageDefinitions do: [ :p | self fileoutPackageDefinition: p ]."
	sortedBehaviors do: [ :c | self fileoutBehavior: c ].
	sortedMethods do: [ :m | self fileoutMethod: m ].
	sortedBehaviors do: [ :c | self fileoutBehaviorInitializer: c ].
]

{ #category : 'private - writing' }
Fileouter >> fileoutBehavior: aBehavior [

	| comment |
	exporter writeClassDefinitionOf: aBehavior.
	exporter writeClassDefinitionOf: aBehavior class.
	
	comment := aBehavior comment.
	(comment isNil or: [comment isEmpty])
		ifFalse: [exporter writeClassCommentOf: aBehavior ]
]

{ #category : 'private - writing' }
Fileouter >> fileoutBehaviorInitializer: aBehavior [

	(aBehavior includesSelector: #initialize)
		ifTrue: [exporter writeClassInitializer: aBehavior]
]

{ #category : 'private - writing' }
Fileouter >> fileoutMethod: aMethod [

	exporter writeMethod: aMethod
]

{ #category : 'API' }
Fileouter >> fileoutOnFileReference: aFileReference [

	aFileReference writeStreamDo: [ :stream | self fileoutOnStream: stream ]
]

{ #category : 'API' }
Fileouter >> fileoutOnStream: aStream [

	exporter := CodeExporterV100 onStream: aStream.
	self fileOut
]

{ #category : 'initialization' }
Fileouter >> initialize [

	super initialize.
	packageDefinitions := Set new.
	behaviors := Set new.
	methods := Set new
]

{ #category : 'accessing' }
Fileouter >> methods [

	^ methods
]

{ #category : 'accessing' }
Fileouter >> methods: anObject [

	methods := anObject
]

{ #category : 'accessing' }
Fileouter >> packageDefinitions [

	^ packageDefinitions
]

{ #category : 'accessing' }
Fileouter >> packageDefinitions: anObject [

	packageDefinitions := anObject
]

{ #category : 'accessing' }
Fileouter >> sortBehaviors [

	^ Package new sortTopologically: self behaviors
]

{ #category : 'private - writing' }
Fileouter >> writeFileoutVersion [

	exporter writeFileoutVersion
]
