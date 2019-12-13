"
I serialize or materialize objects using FUEL framework. Create instance of me calling GTEventPacking fuel.
"
Class {
	#name : #GTFuelPacking,
	#superclass : #GTEventPacking,
	#instVars : [
		'migrations'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #adding }
GTFuelPacking >> addMigration: oneArgBlock [ 
	^ self migrations addLast: oneArgBlock
]

{ #category : #initialization }
GTFuelPacking >> initializeMaterializer: aFLMaterializer [ 
	self migrations do: [ :eachMigration | eachMigration value: aFLMaterializer ]
]

{ #category : #initialization }
GTFuelPacking >> initializeMigrations [
	| pragmas |
	pragmas := Pragma allNamed: #fuelMaterializerMigration in: self class.
	migrations := pragmas
		collect: [ :eachPragma | self perform: eachPragma methodSelector ]
		as: OrderedCollection
]

{ #category : #private }
GTFuelPacking >> materialize: aByteArray [
	| materializer stream gzstream materialization |
	stream := aByteArray asByteArray readStream. 
	gzstream := GZipReadStream on: stream.
	materializer := FLMaterializer newDefault.
	self initializeMaterializer: materializer.
	materialization := materializer materializeFrom: gzstream.
	gzstream close.
	stream close.
	^ materialization root.
]

{ #category : #accessing }
GTFuelPacking >> migrations [
	migrations ifNil: [ self initializeMigrations ].
	^ migrations
]

{ #category : #accessing }
GTFuelPacking >> migrations: aCollectionOrNil [
	migrations := aCollectionOrNil
]

{ #category : #private }
GTFuelPacking >> serialize: anObject [
	| serializer stream gzstream |
	stream := ByteArray new writeStream binary. 
	gzstream := GZipWriteStream on: stream.
	serializer := FLSerializer newDefault.
	serializer at: #recorderVersion putAdditionalObject: self version.
	serializer stream: gzstream.
	serializer serialize: anObject.
	gzstream close.
	stream close.
	^ stream contents.
]

{ #category : #accessing }
GTFuelPacking >> technologyName [
	^ 'fuel'
]
