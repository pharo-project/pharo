An EntryCompletion is a handler for the driving of the completion menu in a PluggableTextFieldMorph. The completion menu is an IdentifierChooserMorph which is typically built and popup when a character is entered in a PluggableTextFieldMorph. 

Instance Variables
	chooseBlock:		<Block>
	chooser:		<IdentifierChooserMorph>
	dataSourceBlock:		<Block>
	filterBlock:		<Block>
	previousToken:		<String>

chooseBlock
	- One argument block which is evaluated when a token is chosen, the token is passed as argument

chooser
	- The IdentifierChooserMorph which is currently opened

dataSourceBlock
	- The block that is evaluated in order to get the list of items

filterBlock
	- The block used to filter the dataSource list, it takes 2 args, the first is an item from the current dataSource list element, the second is the token fetched from the requestor (the PluggableTextFieldMorph). It returns true if the current dataSource list element is to be kept

previousToken
	- Used to be able to not open the list if the current text in the PluggableTextFieldMorph was the previous chosen one
