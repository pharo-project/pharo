private - actions
retrieveUrls: urls ontoQueue: queue withWaitSema: waitSema 
	"download the given list of URLs. The queue will be loaded alternately  
	with url's and with the retrieved contents. If a download fails, the  
	contents will be #failed. If all goes well, a special pair with an empty  
	URL and the contents #finished will be put on the queue. waitSema is  
	waited on every time before a new document is downloaded; this keeps 
	the downloader from getting too far  ahead of the main process"
	"kill the existing downloader if there is one"
	
	self silent 
		ifTrue: [self silentRetrieveUrls: urls ontoQueue: queue withWaitSema: waitSema ]
		ifFalse: [self verboseRetrieveUrls: urls ontoQueue: queue withWaitSema: waitSema ]
