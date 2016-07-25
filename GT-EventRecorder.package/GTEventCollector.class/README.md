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
		occupant: nil "object that represents your application";
		add: 42;
		add: 'Hallo Pharo user';
		yourself.

Example for serious use:
	GTEventCollector new
		category: #testingForReal;
		occupant: nil "object that represents your application, if nil, the collector will be removed from GTEventRecorder object once collected data are sent";
		register; "please do not register just for fun, because we collects those data"
		yourself
 
Internal Representation and Key Implementation Points.

    Instance Variables
	recordedEvents:		<WaitfreeQueue>
	url:		<ZnUrl>
