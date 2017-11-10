"
I represent persistence strategy for GlobalIdentifier. 

I can load or save existing preferences (a dictionary) using #load: or #save: message. I know whare the preference file is stored (#preferences). I also know what previous persistence stategy was used; for that reason you should use #ensure: instead of #load: message. In that case, I will load the old file and then save it using new persistence strategy.

Collaborators: GlobalIdentifier uses me for loading and saving values; in both cases GlobalIdentifier uses #ensure:

Public API and Key Messages

- ensure: it loads stored values, merge with existing in the image and saves the current state on the disk

Example:
	GlobalIdentifierPersistence ston
		previous: (GlobalIdentifierPersistence fuel
					preferences: FileLocator workingDirectory / 'example.fuel';
					yourself);
		preferences: FileLocator workingDirectory / 'example.ston';
		ensure: (Dictionary new at: #a put: 1; at: #b put: 2);
		yourself

Internal Representation and Key Implementation Points.

    Instance Variables
	preferences:				<FileReference>
	previousPersistence:		<GlobalIdentifierPersistence>

"
Class {
	#name : #GlobalIdentifierPersistence,
	#superclass : #Object,
	#instVars : [
		'preferences',
		'previousPersistence',
		'checker'
	],
	#category : #System-Identification
}

{ #category : #'instance creation' }
GlobalIdentifierPersistence class >> fuel [
	^ GlobalIdentifierFuelPersistence new
]

{ #category : #'instance creation' }
GlobalIdentifierPersistence class >> ston [
	^ GlobalIdentifierStonPersistence new
]

{ #category : #accessing }
GlobalIdentifierPersistence >> checker [
	"Return a GlobalIdentifierPersistenceChecker object."
	^ checker ifNil: [ checker := GlobalIdentifierPersistenceChecker new ]
]

{ #category : #accessing }
GlobalIdentifierPersistence >> checker: aGlobalIdentifierPersistenceChecker [
	checker := aGlobalIdentifierPersistenceChecker
]

{ #category : #configuration }
GlobalIdentifierPersistence >> defaultPreferences [
	^ FileLocator preferences / 'pharo' / self fileName
]

{ #category : #operations }
GlobalIdentifierPersistence >> delete [
	"self deletePreferences"
	[ self preferences ensureDelete ]
	on: Error do: [ "ignore" ]
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> ensure: existingDictionary [
	"It proceeds all backward compatibility work."
	self shouldCallPreviousPersistence ifTrue: [ 
		previousPersistence ensure: existingDictionary ].
	self load: existingDictionary
]

{ #category : #operations }
GlobalIdentifierPersistence >> ensureDirectory [
	self preferences parent ensureCreateDirectory 
]

{ #category : #configuration }
GlobalIdentifierPersistence >> fileName [
	^ self subclassResponsibility
]

{ #category : #testing }
GlobalIdentifierPersistence >> hasPreviousPersistence [
	^ previousPersistence notNil
]

{ #category : #testing }
GlobalIdentifierPersistence >> isEnabled [
	"If HOME directory does not exists, ignore the persistence mechanism.
	HOME directory may not exists in case of a server deployment.
	See https://pharo.fogbugz.com/f/cases/19944"
	^ self checker isEnabled
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> load [
	"It loads and returns stored dictionary."
	^ self subclassResponsibility
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> load: existingDictionary [
	"It loads stored information into existingDictionary."
	self isEnabled ifFalse: [ ^ self ].
	self preferences exists ifFalse: [
		"This is a new computer, so we define new computer UUID.
		User still has to agree about sending data if it is not has been done yet."
		^ self save: existingDictionary ].
	[ (self mergeExisting: existingDictionary stored: self load)
			ifTrue: [ self save: existingDictionary ].
	] on: Error do: [ 
		"Preferences likely contains a different settings version, so we store the actual one.
		We should keep the preferences as stable as possible."
		self mayOverwrite: existingDictionary ]
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> mayOverwrite: aDictionary [
	"If I keep previous persistence strategy, I am the main one and I can overwrite the current preference file.
	I am called only on loading failure; it means when I cannot load preference file."
	self hasPreviousPersistence ifTrue: [ self save: aDictionary ].
]

{ #category : #merging }
GlobalIdentifierPersistence >> mergeExisting: existingDictionary stored: storedDictionary [
	^ GlobalIdentifierMerger new
			existing: existingDictionary;
			stored: storedDictionary;
			merge
]

{ #category : #accessing }
GlobalIdentifierPersistence >> preferences [
	^ preferences ifNil: [ preferences := self defaultPreferences ]
]

{ #category : #accessing }
GlobalIdentifierPersistence >> preferences: anObject [
	preferences := anObject
]

{ #category : #accessing }
GlobalIdentifierPersistence >> previous [
	^ previousPersistence
]

{ #category : #accessing }
GlobalIdentifierPersistence >> previous: aGlobalIdentifierPersistence [
	previousPersistence := aGlobalIdentifierPersistence
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> save: aDictionary [
	self isEnabled ifFalse: [ ^ self ].
	self delete.
	self ensureDirectory.
	self preferences writeStreamDo: [ :aPreferenceStream | 
			self save: aDictionary into: aPreferenceStream ]
]

{ #category : #'load and saving' }
GlobalIdentifierPersistence >> save: aDictionary into: aWriteStream [
	"Saves aDictionary into aWriteStream"
	self subclassResponsibility
]

{ #category : #testing }
GlobalIdentifierPersistence >> shouldCallPreviousPersistence [
	^ self preferences exists not and: [ self hasPreviousPersistence ]
]
