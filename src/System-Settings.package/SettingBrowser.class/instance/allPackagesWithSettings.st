accessing
allPackagesWithSettings
	^( self treeHolder nodeList collect: [:n | n receiverPackage]) asSet