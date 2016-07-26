analyzing
traceWith: anAnalysis 

	self clusterReferencesDo: [ :aChild | anAnalysis privateTrace: aChild ]