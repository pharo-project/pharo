pages
analysis
	^HelpTopic 
		title: 'Data Analysis'
		contents:	
	'!! Downloading Data

	The collected data on the server are stored using a standard Linux file system. The data are compressed every hour and can be downloaded from http://gc.dcc.uchile.cl page. 

	You can use GTEventTool class to download all collected data by executing:
	
	GTEventTool default ensureDownloadedData.
	
	It is downloaded to your working directory (next to your Pharo image). Ideally you could extract it using `GTEventTool default ensureExtractedData`, but the current Pharo version does not support ZIP signature that is used for large files. But you can do it manually using standard tools of your system. It is extracted into `./gt/events` directory.
	
!! Data Structure
	
	The data are structured per category and per month. The category is defined by the client side, see the previous section about the client. Let us say that your category is called `myCoolTool` and you have been collecting data since November 2016 to January 2017. Then you will find out the following directory structure:
	
	./gt/events/myCoolTool
		/201611/  - data collected during November 2016
		/201612/  - data collected during Dicember 2016
		/201701/  - data collected during January 2017

	Each file is a serialized GTEventAnnouncement object. The object is a data container of meta data and of collected events.

!! Analysis
	
	We recomend you to remove all data that you are not interested in and start your analysis. To load all data available in the directory executing:
	
	GTEventTool default unpackAll inspect.

	It returns an association `unpackedData -> exceptions`. The key `unpackedData` includes all your data, the value `exceptions` all catched exception that happened while loading the data. You can thus load all correct data and check all errors.
	
	Remember that semantics of the data depends on you. GT Event Recorder only provides a convenient infrustructure to collect data.' 