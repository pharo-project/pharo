(TxTextStyler new
	styleMethod: (TxTextStyler >> #initializeStylesFor:))
	editInWindow


- supports some ast-based navigation:
 	- cmd-click on message selection to show implementors of it
	- shift-cmd-click to show senders of it
	- cmd-click on class name to browse it