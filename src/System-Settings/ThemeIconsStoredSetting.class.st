"
I am responsible for storing information about ThemeIcons objects.
"
Class {
	#name : #ThemeIconsStoredSetting,
	#superclass : #AbstractStoredSetting,
	#instVars : [
		'name',
		'url'
	],
	#category : #'System-Settings-Persistence-Ston-StoredSettings'
}

{ #category : #accessing }
ThemeIconsStoredSetting >> name [
	^ name
]

{ #category : #accessing }
ThemeIconsStoredSetting >> name: anObject [
	name := anObject
]

{ #category : #accessing }
ThemeIconsStoredSetting >> realValue [
	^ ThemeIcons new 
		name: self name;
		url: self url;
		yourself
]

{ #category : #accessing }
ThemeIconsStoredSetting >> url [
	^ url
]

{ #category : #accessing }
ThemeIconsStoredSetting >> url: anObject [
	url := anObject
]
