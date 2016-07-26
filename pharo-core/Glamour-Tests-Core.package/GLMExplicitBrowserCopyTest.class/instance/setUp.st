tests
setUp
	| pane1 pane2 transmission1 transmission2 | 
	browser := GLMExplicitBrowser new. 
	
	pane1 := browser addNewPane name: 'pane1'. 
	pane2 := browser addNewPane name: 'pane2'. 
	transmission1 := GLMTransmission new
		addActiveOrigin: (pane2 port: #selection); 
		destination: (GLMPresentationBoundPort new name: #selection; presentation: browser; yourself); 
		yourself.  
	browser addTransmission: transmission1. 
	transmission2 := GLMTransmission new 
		ensureReplacePresentationsStrategy;
		addActiveOrigin: (pane1 port: #selection); 
		destination: (pane2 port: #entity); 
		addPresentation: (GLMPresentation new title: 'presentation1');
		yourself.  
	browser addTransmission: transmission2. 
	copiedBrowser := browser copy