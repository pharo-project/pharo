Class {
	#name : 'Fileouter',
	#superclass : 'Object',
	#instVars : [
		'behaviors',
		'methods',
		'exporter'
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

	behaviors add: aClass.
	methods addAll: aClass localMethods
]

{ #category : 'API - configuration' }
Fileouter >> addFullPackage: aPackage [

	behaviors addAll: aPackage definedClasses.
	methods addAll: aPackage methods. 
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

	sortedBehaviors do: [ :c | self fileoutBehavior: c ].
	sortedMethods do: [ :m | self fileoutMethod: m ].
	sortedBehaviors do: [ :c | self fileoutBehaviorInitializer: c ].
]

{ #category : 'private - writing' }
Fileouter >> fileoutBehavior: aBehavior [

	| comment |
	exporter writeClassDefinition: aBehavior.
	exporter writeClassDefinition: aBehavior class.
	
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

	exporter := CodeExporter onStream: aStream.
	self fileOut
]

{ #category : 'initialization' }
Fileouter >> initialize [

	super initialize.
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
Fileouter >> sortBehaviors [

	^ Package new sortTopologically: self behaviors
]
