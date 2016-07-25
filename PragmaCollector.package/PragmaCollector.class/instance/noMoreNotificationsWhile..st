system changes
noMoreNotificationsWhile: aBlock
	"don not receive any system change notification during aBloc"
	self noMoreNotifications.
	[ aBlock value ] ensure: [ self installSystemNotifications ]