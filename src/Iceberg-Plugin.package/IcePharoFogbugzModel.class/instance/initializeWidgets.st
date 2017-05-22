initialization
initializeWidgets
	issueNumberText := self newTextInput autoAccept: true.
	issueText := self newTextInput autoAccept: true.
	createButton := self newButton.
	
	
	issueNumberText whenBuiltDo: [ :w | w widget wrapFlag: false ]. 
	issueText whenBuiltDo: [ :w | w widget wrapFlag: false ]. 
	issueNumberText textHolder 
		whenChangedDo: [ :text | self validateIssue: text ].
	
	createButton 
		label: 'Create';
		icon: #add asIcon;
		action: [ self createBranch ].	
	
	self focusOrder 
		add: issueNumberText;
		add: issueText;
		add: createButton