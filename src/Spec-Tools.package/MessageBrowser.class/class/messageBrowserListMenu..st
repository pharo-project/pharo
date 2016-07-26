menu
messageBrowserListMenu: aBuilder
	<contextMenu>
	<messageBrowserListMenu>
	| target model message |
	
	target := aBuilder model.
	model := target model.
	target selectedMessage ifNil: [ ^self ].
	
	message := target selectedMessage compiledMethod.
	
	(aBuilder item: #'Browse full...')
		keyText: 'b';
		action: [ 
			Smalltalk tools browser 
				openOnClass: message methodClass
				selector: message selector ].

	(aBuilder item: #'Inspect method...')
		keyText: 'i';
		action: [ message inspect ];
		withSeparatorAfter.

	(aBuilder item: #'Remove method...')
		keyText: 'x';
		action: [ 
			model 
				removeMethod: message 
				inClass: message methodClass ].

	(aBuilder item: #'Senders of...')
		keyText: 'n';
		action: [ model browseSendersOfMessagesFrom: message selector ].

	(aBuilder item: #'Implementors of...')
		keyText: 'm';
		action: [ model browseMessagesFrom: message selector ].

	(aBuilder item: #'Users of...')
		keyText: 'N';
		action: [ model browseClassRefsOf: message methodClass ].
	
	(aBuilder item: #'Versions...')
		keyText: 'v';
		action: [ model browseVersionsFrom: message ].		
