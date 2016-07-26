menus
methodListMenu: aMenu
	"Build the menu for the selected method, if any."
	
	self selectedMessageName 
		ifNil: [ items notEmpty ifTrue: [aMenu add: 'FileOut (o)'	selector: #fileOutMessage]]
		ifNotNil: [
			aMenu addList:#(
				('Browse full (b)' 						browseMethodFull)
				('Browse hierarchy (h)'					classHierarchy)
				-
				('FileOut (o)'							fileOutMessage)
				('Copy selector (c)'						copySelector)).
			aMenu addList: #(
				-
				('Browse senders (n)'						browseSendersOfMessages)
				('Browse implementors (m)'					browseMessages)
				('Inheritance (i)'						methodHierarchy)
				('Versions (v)'							browseVersions)) ].
	^ aMenu
