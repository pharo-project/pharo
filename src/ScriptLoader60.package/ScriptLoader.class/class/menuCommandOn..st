menu
menuCommandOn: aBuilder 
	<worldMenu>
	
	(aBuilder item: #'Integrator Menu')
		order: 1.5;
		precondition: [self currentlyIntegratingChanges];
		parent: #System;
		icon: (Smalltalk ui icons iconNamed: #smallAuthoringToolsIcon);
		action: [ScriptLoader releaseMenu]