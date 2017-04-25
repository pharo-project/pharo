pages
client
	^HelpTopic 
		title: 'Client'
		contents:	 (String streamContents: [ :aStream | aStream 
nextPutAll: 
'	The client includes four main objects: GTEventRecorder, GTEventCollector, GTEventAnnouncement, and GTEventTool. The last one is described in the Data Analysis section.
		
!! GTEventRecorder

	GTEventRecorder is a singleton that is responsible for collecting and sending data to a server. You, as a GT Event Recorder user, do not need to take care much of this object. Once you register your GTEventCollector object, it does the job.
	
	It sends data to the server every twenty minutes. If you want to send data immediately, you can execute `GTEventRecorder uniqueInstance deliverNow`.

	Here is the class description:';
nextPutAll: GTEventRecorder comment;
cr;
nextPutAll: 
'!! GTEventCollector

	GTEventCollector is a data collector which you want to use. You can create a new instance, register it to the GTEventRecorder and starts to collect data. Here is a basic example:
	
	GTEventCollector new
		category: #testingForFun; 	"the data are stored under the defined category."
		occupant: nil; 					"object that represents your application, if nil, the collector will be removed from GTEventRecorder object once collected data are sent"
		add: 42; 							"collected data"
		add: ''Hello Pharo user''; 	"collected data"
		register; 						"please, be aware that we collect the data"
		yourself.

	As you can see, you can collect any data that is possible to serialize. The current version uses STON for data serialization, see GTStonPacking class. 
	
	Before the data are sent to the server, it is serialized and kept by an GTEventAnnouncement object who also keeps additional information, e.g., computer-id, data and time. The additional information helps you to group collected data per computer and order them according to time. For that reason, we recommend you to collect objects that responds to #infoAt:put:. If so, the additional information is passed to your extracted data.  GTBasicRecorderEvent class represents a basic example that we recommend to use, e.g., subclass it. 

	Here is the class description:';
nextPutAll: GTEventCollector comment ])