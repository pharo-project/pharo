pages
server
	^HelpTopic 
		title: 'Server'
		contents:	
	'	The server is not included in the standard Pharo image. You can find it in http://smalltalkhub.com/#!/~Moose/GToolkit repository, package GT-EventRecorder-Server, or load it using the following configuration:
	
	Gofer it
		smalltalkhubUser: #Moose project: #GToolkit;
		configurationOf: #GTEventRecorderServer;
		loadStable.
	
	The server uses standard Zinc Server (ZnServer class) and you can start it by executing:
	
	GTEventRecorderServer start.
	
	By default, it runs on port 8080. If you need to run it with other services, check GTEventRecorderDelegate class.
	
	You can stop the server using: 
	
	GTEventRecorderServer stop.
	
	The standard server is executed on http://gc.dcc.uchile.cl:8080. 
	
	The following section describes the data storage and data analysis.
	' 