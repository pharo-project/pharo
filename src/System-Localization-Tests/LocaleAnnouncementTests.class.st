Class {
	#name : #LocaleAnnouncementTests,
	#superclass : #TestCase,
	#instVars : [
		'saveCurrent',
		'catchedAnnouncement'
	],
	#category : 'System-Localization-Tests'
}

{ #category : #'private accessing' }
LocaleAnnouncementTests >> frenchLocale [
	^ Locale isoLanguage: 'fr'
]

{ #category : #events }
LocaleAnnouncementTests >> localeWasChanged: anAnnouncement [

	catchedAnnouncement := anAnnouncement 
]

{ #category : #running }
LocaleAnnouncementTests >> setUp [

	super setUp.
	saveCurrent := Locale current.
	Locale localeAnnouncer when: LocaleChanged send: #localeWasChanged: to: self
]

{ #category : #running }
LocaleAnnouncementTests >> tearDown [

	Locale localeAnnouncer unsubscribe: self.
	Locale classVarNamed: 'Current' put: saveCurrent.
	super tearDown
]
