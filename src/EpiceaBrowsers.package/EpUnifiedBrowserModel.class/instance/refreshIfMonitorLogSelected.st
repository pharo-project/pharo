refreshing
refreshIfMonitorLogSelected
	"See initializeWorkaroundToRefreshOnMonitorLogAnnouncement"
	
	| item |
	item := logNodesModel logNodesTreeModel selectedItem.
	item ifNil: [ ^self ]. "Nothing to refresh"
	item content isMonitor ifFalse: [ ^self ]. "Monitor log isn't selected"
	logModel refresh.