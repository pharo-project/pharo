"
I represent a collection of data that should be sent to a specific URL. 

I am responsible for collecting data (#add: method) and for giving collected data to GTEventPacking object (#bundle method).

You should consider to set #category. This value is used for separating data on the server to directories. That way you will then easier receive your collected data. If you creates a subclass of me, you can override #defaultCategory method.

You should also consider set #occupant:. This could be an object that uses me and feeds me with data. I keep this occupant weakly. So, if the object is removed from the memory, GTEventRecorder knows that I can be removed too. 

I collaborate with GTEventPacking object that receives collected data and URL, serilize the collected data, and creates GTEventBundle object. My subclasses can handle differently what data to send together to server by rewritting #bundle method.

Public API and Key Messages

- bundle   
- url
- category
- occupant:
- register

Example for playing with me:
	GTEventCollector new
		category: #testingForFun;
		occupant: nil ""object that represents your application"";
		add: 42;
		add: 'Hallo Pharo user';
		yourself.

Example for serious use:
	GTEventCollector new
		category: #testingForReal;
		occupant: nil ""object that represents your application, if nil, the collector will be removed from GTEventRecorder object once collected data are sent"";
		register; ""please do not register just for fun, because we collects those data""
		yourself
 
Internal Representation and Key Implementation Points.

    Instance Variables
	recordedEvents:		<WaitfreeQueue>
	url:		<ZnUrl>

"
Class {
	#name : #GTEventCollector,
	#superclass : #Object,
	#instVars : [
		'url',
		'category',
		'recordedEvents',
		'registry',
		'updateAnnouncementBlock'
	],
	#category : #'GT-EventRecorder-Core'
}

{ #category : #examples }
GTEventCollector class >> notRegisteredCollector [
	<gtExample>
	<label: 'It collects an integer and a string'>
	^ self new
		category: #testingForFun; "the data are stored under the defined category."
		occupant: nil; "object that represents your application, if nil, the collector will be removed from GTEventRecorder object once collected data are sent"
		add: 42; "collected data"
		add: 'Hallo Pharo user'; "collected data"
		register; "please, be aware that we collect the data"
		yourself.
]

{ #category : #adding }
GTEventCollector >> add: anEvent [ 
	"And an event (any object) that you want to send to a server."
	^ recordedEvents nextPut: anEvent
]

{ #category : #adding }
GTEventCollector >> addIfAvailable: anEvent [
	"It add event to a queue only if sending diagnostic and usage data is enabled."
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ anEvent ].	
	^ self add: anEvent
]

{ #category : #accessing }
GTEventCollector >> bundle [
	"It returns all collected events that are supposed to be sent. If you need to send just a portion of the collected events, this is the place to overwrite."
	^ self bundle: OrderedCollection new.
]

{ #category : #accessing }
GTEventCollector >> bundle: aCollection [
	recordedEvents flush: [ :eachEvent | aCollection addLast: eachEvent ].
	^ aCollection
]

{ #category : #accessing }
GTEventCollector >> category [
	^ category ifNil: [ category := self defaultCategory ]
]

{ #category : #accessing }
GTEventCollector >> category: aString [ 
	"It is used for distinguishing data from different application on a server.
	The data are stored separately on a server according to category name."
	^ category := aString
]

{ #category : #configuration }
GTEventCollector >> defaultCategory [
	"Default category name. You can set your own by calling #category: or 
	by overwriting this method if you have a spetialized subclass."
	^ #unknown
]

{ #category : #accessing }
GTEventCollector >> flush [
	"Be careful when using it. You will lost all data."
	recordedEvents flush
]

{ #category : #'gt-inspector-extension' }
GTEventCollector >> gtInspectorActionHelp [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [:b | 
		HelpBrowser open selectTopicSatisfying: [:each |
			each owner notNil and:  [
			(each owner key = GTEventRecorderHelp key) and: [
				each title = 'Client' ] ] ] ];
		icon: GLMUIThemeExtraIcons glamorousHelp;
		title: 'Help'
]

{ #category : #initialization }
GTEventCollector >> initialize [
	super initialize.	
	recordedEvents := WaitfreeQueue new.
]

{ #category : #testing }
GTEventCollector >> isLiberated [
	"When I answer true, I can be removed from a GTEventRecorder object."
	^ self registry isLiberated and: [ recordedEvents isEmpty ]
]

{ #category : #actions }
GTEventCollector >> liberate [
	"Liberate me from the occupant so I can be removed from GTEventRecorder object in the next delivery cycle."
	^ self registry liberate.
]

{ #category : #accessing }
GTEventCollector >> occupant: anObject [ 
	"anObject sends me data. I weakly keeps anObject and once it is released, I can be removed from GTEventRecorder object."
	self registry object: anObject
]

{ #category : #printing }
GTEventCollector >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPut: $[;
		print: recordedEvents size;
		space;
		nextPutAll: ('item' asPluralBasedOn: recordedEvents size);
		nextPut: $]
]

{ #category : #initialization }
GTEventCollector >> register [
	"Register to the global GTEventRecorder."
	GTEventRecorder uniqueInstance addCollector: self.
]

{ #category : #accessing }
GTEventCollector >> registry [
	"Registry is a weak holder of my owner. Once it is nil, I can be removed from GTEventRecorder object."
	^ registry ifNil: [ registry := GTEventRegistry new ]
]

{ #category : #accessing }
GTEventCollector >> size [
	"It returns number of collected events."
	^ recordedEvents size
]

{ #category : #updating }
GTEventCollector >> updateAnnouncement: aGTEventAnnouncement [ 
	updateAnnouncementBlock ifNil: [ ^ aGTEventAnnouncement ].
	updateAnnouncementBlock cull: aGTEventAnnouncement.
	^ aGTEventAnnouncement
]

{ #category : #updating }
GTEventCollector >> updateAnnouncementBlock: aBlockClosure [ 
	updateAnnouncementBlock := aBlockClosure
]

{ #category : #accessing }
GTEventCollector >> url [
	"It returns URL indicating where to send collected data."
	^ url ifNil: [ url := GTEventRecorderSettings url ]
]

{ #category : #accessing }
GTEventCollector >> url: aZnUrl [
	"URL that indicates where to send collected data."
	url := aZnUrl
]
