action
installMetalink
	|link|
	
	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.

	compiledMethods do:[:method| method ast nodesDo: [:node | node link: link]].