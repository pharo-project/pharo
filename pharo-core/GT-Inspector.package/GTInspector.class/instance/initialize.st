initialization
initialize
	super initialize.
	
	self presentationFilter: (GTInspectorTagFilter new
			defaultTag: #custom;
			addAndSelectTag: #basic;
			addAndSelectTag: #custom;
			addAndSelectTag: #examples)