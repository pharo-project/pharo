visiting
visitGlobalSend: anObject name: globalName selector: aSymbol

	(self clusterKeyedByClusterName: FLGlobalSendCluster)
		add: anObject name: globalName selector: aSymbol traceWith: analysis
