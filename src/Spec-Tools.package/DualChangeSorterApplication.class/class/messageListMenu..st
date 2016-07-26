menu
messageListMenu: aBuilder
	<contextMenu>
	<dualChangeSorterMessageListMenu>
	
	| target src dest |
	
	self flag: #todo. "I do not like how I'm getting the parameters (an Array), but looks the faster way now :(
	And I need src and dest parameters because it is called in two sides of change sorter"
	target := aBuilder model first.
	src := aBuilder model second.
	dest := aBuilder model third. 
	
	(aBuilder item: #'Copy method to the other change set')
		action: [ target copyMethodFrom: src to: dest ].
	(aBuilder item: #'Move method to the other change set')
		action: [ target moveMethodFrom: src to: dest ];
		withSeparatorAfter.
