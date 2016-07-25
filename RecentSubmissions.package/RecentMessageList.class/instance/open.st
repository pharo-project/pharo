userinterface
open
	"opens the system window for recent messages"

	^ MessageBrowser new
		topologicSort: false;
		sortingBlock: [ :a :b | a timeStamp > b timeStamp ];
		messages: methodReferenceList;
		title: 'Recently accepted Methods' translated;
		open