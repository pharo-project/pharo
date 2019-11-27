"
I keep all GTEventBundle objects that should be delivered to the same remote server. That way, all data can be sent to the server at one HTTP request.

By calling #entity, you receive ZnMultiPartFormDataEntity object that contains ZnMimePart objects in a sequence 'category', 'data', 'category', 'data', etc. Server is then responsible for spliting this object into the parts and store each data in to a right position (directory).

For the Collaborators Part: 
- GTEventBundle: I keep collection of those objects and asks them for a ZnEntity object calling #entity.
- GTEventMultiBundleFactory knows how to create instances.
- GTEventDelivery knows how to send my #entity to a server.

Internal Representation and Key Implementation Points.

    Instance Variables
	bundles:		<Collection>
	url:			<ZnUrl>

"
Class {
	#name : #GTEventMultiBundle,
	#superclass : #Object,
	#instVars : [
		'url',
		'bundles'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #visitor }
GTEventMultiBundle >> accept: aVisitor [ 
	^ aVisitor visitMultiBundle: self
]

{ #category : #adding }
GTEventMultiBundle >> add: aGTEventBundle [ 
	^ self bundles addLast: aGTEventBundle
]

{ #category : #accessing }
GTEventMultiBundle >> bundles [
	^ bundles ifNil: [ bundles := OrderedCollection new ]
]

{ #category : #accessing }
GTEventMultiBundle >> entity [
	| entity |
	entity := ZnMultiPartFormDataEntity new.
	self updateEntity: entity.
	^ entity
]

{ #category : #private }
GTEventMultiBundle >> updateEntity: aZnMultiPartFormDataEntity [ 
	bundles ifNil: [ ^ self ].
	self bundles do: [ :eachEventBundle |
		aZnMultiPartFormDataEntity 
			addPart: (ZnMimePart fieldName: 'category' value: eachEventBundle category);
			addPart: (ZnMimePart fieldName: 'data' entity: eachEventBundle entity) ]
]

{ #category : #accessing }
GTEventMultiBundle >> url [
	^ url
]

{ #category : #accessing }
GTEventMultiBundle >> url: anObject [
	url := anObject
]
