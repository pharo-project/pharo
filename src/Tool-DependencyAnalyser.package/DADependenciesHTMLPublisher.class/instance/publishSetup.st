publishing
publishSetup
	| body |
	body := String streamContents: [ :str |
		str
			<< 'Image version: <span class="label label-primary">';
			<< self imageVersionString; 
			<< '</span></br> Analysis run on ';
			<< report analysisRunString ; 
			<< '</br>' ].
	self accordion: 'accordionSetup' name: 'Setup' headingContent: 'Analysis setup' body: body