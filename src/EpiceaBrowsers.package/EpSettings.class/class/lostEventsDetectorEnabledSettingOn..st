system settings
lostEventsDetectorEnabledSettingOn: aBuilder

	<systemsettings>
	(aBuilder setting: #lostEventsDetectorEnabled)
		label: 'Detect lost events on start-up';
		description: 'Check if current Epicea log has events that were not applied in this image (useful for recovegin changes when the image crashed)';
		parent: #epicea;
		target: self.