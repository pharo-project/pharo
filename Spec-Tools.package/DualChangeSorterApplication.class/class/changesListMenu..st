menu
changesListMenu: aBuilder
	<contextMenu>
	<dualChangeSorteChangesListMenu>
	
	| target src dest |

	self flag: #todo. "I do not like how I'm getting the parameters (an Array), but looks the faster way now :(
	And I need src and dest parameters because it is called in two sides of change sorter"
	target := aBuilder model first.
	src := aBuilder model second.
	dest := aBuilder model third. 
	
	src selectedChangeSet ifNil:[ ^self ].
	
	(aBuilder item: #'Copy all to the other change set')
		keyText: 'c';
		action: [ target copyAllFrom: src to: dest ].
	(aBuilder item: #'Submerge into the other change set')
		action: [ target submerge: src into: dest ].
	(aBuilder item: #'Subtract the other change set')
		keyText: '-';
		action: [ target subtractFrom: src to: dest ];
		withSeparatorAfter.