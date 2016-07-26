event handling
textChanged
	"this is used in the case where we want to listen for live changes in a text morph. otherwise we're limited to the accept behavior"
	
	changedAction ifNotNil: [changedAction value: textMorph contents asText].