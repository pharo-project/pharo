protocol
actionOnAddPackage
	self buttonAddPackage action: [ (DAAddPackageModel 
					new 
					packageAnalyzerTreeModel: self)
					open ]