"
My class-side protocol initializes System Settings' stuff.
"
Class {
	#name : #EpSettings,
	#superclass : #Object,
	#category : #'EpiceaBrowsers-Integration'
}

{ #category : #'system settings' }
EpSettings class >> groupSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #epicea)
		label: 'Epicea';
		description: 'Epicea is a tool that records code changes enriched with IDE events. Epicea extends and could replace the traditional .changes logging mechanism, where the recorded code changes are incomplete and are not properly reified and thus it can be difficult to recover lost code after an image crash.';
		parent: #tools.
]

{ #category : #accessing }
EpSettings class >> lostEventsDetectorEnabled [

	^ EpLostChangesDetector isEnabled
]

{ #category : #accessing }
EpSettings class >> lostEventsDetectorEnabled: aBoolean [

	EpLostChangesDetector enabled: aBoolean
]

{ #category : #'system settings' }
EpSettings class >> lostEventsDetectorEnabledSettingOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #lostEventsDetectorEnabled)
		label: 'Detect lost events on start-up';
		default: true;
		description: 'Check if current Epicea log has events that were not applied in this image (useful for recovegin changes when the image crashed)';
		parent: #epicea;
		target: self.
]

{ #category : #accessing }
EpSettings class >> monitorEnabled [

	^ EpMonitor current isEnabled
]

{ #category : #accessing }
EpSettings class >> monitorEnabled: aBoolean [

	EpMonitor current enabled: aBoolean
]

{ #category : #'system settings' }
EpSettings class >> monitorEnabledSettingOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #monitorEnabled)
		label: 'Monitor enabled';
		default: true;
		description: 'The Epicea Monitor listens events in the system such as class added and method modified, and records them to the current Epicea log.';
		parent: #epicea;
		target: self.
]

{ #category : #accessing }
EpSettings class >> storeNameStrategyClass [

	^ OmSessionStore storeNameStrategy class
]

{ #category : #accessing }
EpSettings class >> storeNameStrategyClass: aStrategyClass [

	OmSessionStore storeNameStrategy: aStrategyClass new
]

{ #category : #'system settings' }
EpSettings class >> storeNameStrategySettingsOn: aBuilder [

	<systemsettings>
	(aBuilder pickOne: #storeNameStrategyClass)
		label: 'File Naming Strategy';
		description: 'Choose how will be the Epicea logs named.';
		parent: #epicea;
		target: self;
		default: OmRandomSuffixStrategy;
		domainValues: OmSessionStoreNamingStrategy availableStrategies.

]

{ #category : #accessing }
EpSettings class >> writingDeferDuration [

	^ EpMonitor current writingDeferDuration asMilliSeconds
]

{ #category : #accessing }
EpSettings class >> writingDeferDuration: aSmallInteger [

	^ EpMonitor current writingDeferDuration: aSmallInteger milliSeconds
]

{ #category : #'system settings' }
EpSettings class >> writingDeferDurationSettingOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #writingDeferDuration)
		label: 'Writing defer duration';
		default: 250;
		description: 'Time to wait before write. When Epicea listens new events, it does not write them immediately. Instead, the new events are buffered until there were not new events during the time specified by this setting. The goal is to optimize I/O operations by batching them. 
		
Consecuences of a high defer duration: 
- Pros: For example, when a project is loaded into the image, many events happen in a small window of time.
- Cons: Code changes can be lost if the vm crashes.';
		parent: #epicea;
		target: self.
]
