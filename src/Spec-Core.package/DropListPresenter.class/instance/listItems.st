api
listItems
	"<api: #inspect>"
	"Return the list used to populate the drop list"
	
	^ listHolder value collect: [ :e | e model ].