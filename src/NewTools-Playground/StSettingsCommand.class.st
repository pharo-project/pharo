Class {
	#name : #StSettingsCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StSettingsCommand class >> defaulDescription [

	^ 'Open settings browser.'
]

{ #category : #accessing }
StSettingsCommand class >> defaultIconName [

	^ #configuration
]

{ #category : #default }
StSettingsCommand class >> defaultName [

	^ 'Settings'
]

{ #category : #execution }
StSettingsCommand >> execute [ 

	SettingBrowser new 
		changePackageSet: { (RPackageOrganizer default packageNamed: 'GT-Playground') }; 
		open; 
		expandAll	
]
