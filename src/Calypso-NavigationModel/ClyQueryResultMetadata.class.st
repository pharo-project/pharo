"
I represent metadata of query result as collection of first class properties (kind of ClyProperty).
I allow annotate query result with summary information about items. 

For example SUnit plugin annotates method queries with information about tests.
Another example: ClyAsyncQueryResult includes metadata ClyBackgroundProcessingTag which indicates that result is still in processing. 

To access properties use following methods: 
- addProperty: aProperty
- getProperty: aPropertyClass
- hasProperty: aPropertyClass

Metadata is collected lazely when user asks it from the result: 
	aQueryResult metadata 

Actual logic to collect metadata is implemented by environment plugins. But concrete dispatch method is choosen by query which built given result:

	metadata := ClyQueryResultMetadata new.
	environment pluginsDo: [:each | 
		buildingQuery collectMetadataOf: self by: each	] 

Query sends typed message depending on items which query retrieves. For example:
	
	ClyMethodQuery>>collectMetadataOf: aQueryResult by: anEnvironmentPlugin
		anEnvironmentPlugin collectMetadataOfMethods: aQueryResult

So metadata is property of query result. But when you open browser cursor metadata is passed to the cursor instances. It is important optimization for remote scenario where result is remote proxy. In that case cursor is transferred to the client by value together with metadata. So all properties are available for the local cursor user.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	properties:	<OrderedCollection of: <ClyProperty>>
"
Class {
	#name : #ClyQueryResultMetadata,
	#superclass : #Object,
	#instVars : [
		'properties'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #accessing }
ClyQueryResultMetadata >> addProperty: aProperty [
	properties add: aProperty
]

{ #category : #accessing }
ClyQueryResultMetadata >> getProperty: aPropertyClass [ 

	^properties detect: [ :each | each isKindOf: aPropertyClass ] 
]

{ #category : #testing }
ClyQueryResultMetadata >> hasProperty: aPropertyClass [

	^properties anySatisfy: [ :each | each isKindOf: aPropertyClass ]
]

{ #category : #initialization }
ClyQueryResultMetadata >> initialize [
	super initialize.
	
	properties := OrderedCollection new
]

{ #category : #accessing }
ClyQueryResultMetadata >> properties [
	^ properties
]

{ #category : #accessing }
ClyQueryResultMetadata >> properties: anObject [
	properties := anObject
]

{ #category : #accessing }
ClyQueryResultMetadata >> removeProperty: aProperty [
	properties remove: aProperty ifAbsent: [  ]
]

{ #category : #accessing }
ClyQueryResultMetadata >> reset [
	properties removeAll
]
