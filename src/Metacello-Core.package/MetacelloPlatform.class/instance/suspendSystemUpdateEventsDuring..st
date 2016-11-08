system
suspendSystemUpdateEventsDuring: aBlock
	"Wraps call to the system evetn manager to tell it that we want to suspend events during execution of aBlock"
	(self class environment at: #SystemChangeNotifier) uniqueInstance doSilently: aBlock