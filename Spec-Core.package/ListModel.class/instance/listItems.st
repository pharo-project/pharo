api
listItems
	"<api: #inspect>"
	"Return the items of the list. They are your domain specific items"
	
	^ listItemsCache ifNil: [ listItemsCache :=  filteringBlockHolder value value: listHolder value ]
	