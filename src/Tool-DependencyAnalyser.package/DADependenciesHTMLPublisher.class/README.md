This class generates an HTML report of system dependencies in HTML format.

Example: 

 '/tmp/report.html' asFileReference writeStreamDo: [ :stream |
	 DADependenciesHTMLPublisher 
		publishReportFrom: (DADependencyChecker new transitiveDependenciesOf: #Kernel)
		stream: stream ]