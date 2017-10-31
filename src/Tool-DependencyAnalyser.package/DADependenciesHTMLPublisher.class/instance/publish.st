publishing
publish
	self 
		publishHTMLHeader;
		publishSetup;
		publishWarnings;
		publishDependenciesCaption;
		publishDependencies;
		publishDependenciesCaption;
		publishHTMLFooter.
	stream close.