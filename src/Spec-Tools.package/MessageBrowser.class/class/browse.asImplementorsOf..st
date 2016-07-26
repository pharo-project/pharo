instance creation
browse: aCollection asImplementorsOf: aSymbol 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		setRefreshingBlockForImplementorsOf: aSymbol;
		messages: aCollection;
		title: 'Implementors of ', aSymbol printString;
		open