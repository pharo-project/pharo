publishing
publishReportFrom: aCGODependencyReport stream: aStream 
	self basicNew 
		initializeWithReport: aCGODependencyReport stream: aStream;
		publish
		