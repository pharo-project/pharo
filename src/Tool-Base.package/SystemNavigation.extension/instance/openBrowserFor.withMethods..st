*Tool-Base
openBrowserFor: aLiteral withMethods: aCollection
	"Create and schedule a message sender browser for aCollection which normally should come from a query based on aLiteral (senders, implementors...). This method is usefull to avoid to call twice allCallsOn: in certain occasion.
	For example, 
		| sys |
		sys := SystemNavigation new.
		sys 
			openBrowserFor: #printOn: 
			withMethods: (sys allCallsOn: #printOn:) asSortedCollection"

	^ self headingAndAutoselectForLiteral: aLiteral do:
		[:label :autoSelect|
		self
			browseMessageList: aCollection
			name: label
			autoSelect: autoSelect
			refreshingBlock: [ :method | method hasSelector: aLiteral ] ]