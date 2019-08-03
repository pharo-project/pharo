"
Unit test locale announcements
"
Class {
	#name : #LocaleAnnouncementTest,
	#superclass : #TestCase,
	#instVars : [
		'saveCurrent',
		'catchedAnnouncement'
	],
	#category : #'System-Localization-Tests-Announcements'
}

{ #category : #'private accessing' }
LocaleAnnouncementTest >> frenchLocale [

	^ Locale isoLanguage: 'fr'
]

{ #category : #events }
LocaleAnnouncementTest >> localeWasChanged: anAnnouncement [

	catchedAnnouncement := anAnnouncement 
]

{ #category : #running }
LocaleAnnouncementTest >> setUp [

	super setUp.
	saveCurrent := Locale current.
	Locale localeAnnouncer when: LocaleChanged send: #localeWasChanged: to: self
]

{ #category : #running }
LocaleAnnouncementTest >> tearDown [

	Locale localeAnnouncer unsubscribe: self.
	Locale classVarNamed: 'Current' put: saveCurrent.
	super tearDown
]
