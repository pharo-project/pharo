"
Announce that a locale was changed
"
Class {
	#name : #LocaleChanged,
	#superclass : #Announcement,
	#instVars : [
		'newLocale',
		'oldLocale'
	],
	#category : #System-Localization-Locales
}

{ #category : #'instance creation' }
LocaleChanged class >> from: oldLocale to: newLocale [

	^(self new)
		oldLocale: oldLocale;
		newLocale: newLocale;
		yourself
]

{ #category : #accessing }
LocaleChanged >> newLocale [
	^ newLocale
]

{ #category : #accessing }
LocaleChanged >> newLocale: anObject [
	newLocale := anObject
]

{ #category : #accessing }
LocaleChanged >> oldLocale [
	^ oldLocale
]

{ #category : #accessing }
LocaleChanged >> oldLocale: anObject [
	oldLocale := anObject
]
