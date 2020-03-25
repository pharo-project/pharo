"
Settings for the system itself
"
Class {
	#name : #SystemSystemSettings,
	#superclass : #Object,
	#classVars : [
		'SoundEnabled',
		'UseThemeSounds'
	],
	#category : #'Settings-System'
}

{ #category : #settings }
SystemSystemSettings class >> systemSettingOn: aBuilder [
	<systemsettings>
	
	(aBuilder group: #tools)
		label: 'Tools';
		description: 'Settings related to Tools'.
	
	(aBuilder group: #SCM)
		label: 'Software Configuration Management';
		parent: #tools;
		description: 'Settings related to version management and code sharing'.
	
	(aBuilder group: #pharoSystem) 
		label: 'System';
		description: 'General system settings' ;
		with: [
			(aBuilder setting: #serverMode)
				label: 'Server mode';
				target: WorldState;
				default: false;
				description: 'If enabled, then each main rendering loop will always wait a complete delay of 50 ms' .
			(aBuilder setting: #useLocale)
				label: 'Use locale';
				default: false;
				target: Locale;
				selector: #activated;
				description: 'Use the system locale to set the system language at startup.']

]
