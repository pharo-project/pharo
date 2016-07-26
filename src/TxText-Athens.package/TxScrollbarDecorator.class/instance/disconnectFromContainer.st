misc
disconnectFromContainer
	"unregister from all boring stuff.. one day"
	scrollbar ifNotNil: [
		 scrollbar announcer unsubscribe: self ]. 