private - actions
silentRetrieveUrls: urls ontoQueue: queue withWaitSema: waitSema 
	"download the given list of URLs. The queue will be loaded alternately  
	with url's and with the retrieved contents. If a download fails, the  
	contents will be #failed. If all goes well, a special pair with an empty  
	URL and the contents #finished will be put on the queue. waitSema is  
	waited on every time before a new document is downloaded; this keeps 
	the downloader from getting too far  ahead of the main process"
	"kill the existing downloader if there is one"
	
	self zapUpdateDownloader.
	"fork a new downloading process"
	UpdateDownloader := [
		urls do: [ :url | | doc | 
			waitSema wait.
			queue nextPut: url.
			doc := ZnClient new
				systemPolicy;
				setAcceptEncodingGzip;
				beOneShot;
				ifFail: [ :exception | 
					queue nextPut: #failed.
					UpdateDownloader := nil.
					Processor activeProcess terminate ];
				get: url.
			queue nextPut: doc readStream ].		
		queue nextPut: ''.
		queue nextPut: #finished.
		UpdateDownloader := nil] newProcess.
	UpdateDownloader priority: Processor userInterruptPriority.
	"start the process running"
	UpdateDownloader resume