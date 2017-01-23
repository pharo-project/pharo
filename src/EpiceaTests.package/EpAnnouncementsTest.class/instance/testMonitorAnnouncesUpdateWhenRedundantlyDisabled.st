tests
testMonitorAnnouncesUpdateWhenRedundantlyDisabled

	monitor disable.
	self assertMonitorAnnouncesUpdateWhen: [ monitor disable ].
