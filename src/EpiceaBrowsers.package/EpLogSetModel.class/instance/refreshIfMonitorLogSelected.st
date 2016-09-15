refreshing
refreshIfMonitorLogSelected
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"
	
	| item |
	item := logNodesModel logNodesTreeModel selectedItem.

	(logModel log isNil or: [ item isNil ]) ifTrue: [ ^self ]. "Nothing to refresh"
	self flag: #todo. "Fix: breaking encapsulation"
	(monitorLog store store globalName = item content globalName) ifFalse: [ ^self ]. "Monitor log isn't selected"

	logModel log refresh. "Manually refresh log since it is inicially not read"
	logModel refresh. "Refresh UI"