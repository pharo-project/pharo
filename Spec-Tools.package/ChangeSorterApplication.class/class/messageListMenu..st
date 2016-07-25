menu
messageListMenu: aBuilder
	<contextMenu>
	<changeSorterMessageListMenu>

	(aBuilder item: #'Delete method from changeSet')
		keyText: 'd';
		selector: #forgetMessage.
	(aBuilder item: #'Remove method from system')
		keyText: 'x';
		selector: #removeMessage;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b';
		selector: #browseMethodFull;
		withSeparatorAfter.
		
	(aBuilder item: #'FileOut')
		selector: #fileOutMessage.
	(aBuilder item: #'Senders of...')
		keyText: 'n';
		selector: #browseSendersOfMessages.
	(aBuilder item: #'Implementors of...')
		keyText: 'm';
		selector: #browseImplementorsOfMessages.
	(aBuilder item: #'Versions')
		keyText: 'v';
		selector: #browseVersions.
