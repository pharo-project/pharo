system settings
writingDeferDurationSettingOn: aBuilder

	<systemsettings>
	(aBuilder setting: #writingDeferDuration)
		label: 'Writing defer duration';
		description: 'Time to wait before write. When Epicea listens new events, it does not write them immediately. Instead, the new events are buffered until there were not new events during the time specified by this setting. The goal is to optimize I/O operations by batching them. 
		
Consecuences of a high defer duration: 
- Pros: For example, when a project is loaded into the image, many events happen in a small window of time.
- Cons: Code changes can be lost if the vm crashes.';
		parent: #epicea;
		target: self.