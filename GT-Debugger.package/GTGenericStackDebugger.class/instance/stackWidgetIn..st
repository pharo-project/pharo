building presentations
stackWidgetIn: composite

	^ composite fastTable	
		title: 'Stack';
		display: [:aSession | self retrieveStackFrom: aSession];
		column: 'Class' 
			evaluated: [ :aContext | self formatStackClassColumnForContext: aContext]
			width: 300;
		column: 'Method' 
			evaluated: [ :aContext | self formatStackMethodColumnForContext: aContext ];
		column: 'Other' 
			evaluated: [ :aContext | self formatStackExtraColumnForContext: aContext ];
		hideHeader