"
I am an AnnouncementSet that explicitely does not handle a number of exclusion Announcements.

You could define me by #- message:

	announcer when: Announcement - ValueChanged do: [...].
	announcer when: Announcement - ValueChanged - ValueAdded do: [...].
	announcer when: ValueAdded, ValueRemoved - ValueAdded
 
Internal Representation and Key Implementation Points.

    Instance Variables
	exclusions:		<OrderedCollection>
"
Class {
	#name : #AnnouncementSetWithExclusions,
	#superclass : #AnnouncementSet,
	#instVars : [
		'exclusions'
	],
	#category : #'Announcements-Core-Collections'
}

{ #category : #private }
AnnouncementSetWithExclusions >> addExclusion: anAnnouncement [
	"Add anAnnouncement as an exclusion to me.
	I will explicitly not handle my exclusion announcements."

	^ exclusions add: anAnnouncement
]

{ #category : #testing }
AnnouncementSetWithExclusions >> handlesAnnouncement: anAnnouncement [
	"Return true when I will handle anAnnouncement.
	I extend my superclass behavior by explicitly
	not handling the exclusions."

	^ (super handlesAnnouncement: anAnnouncement)
			and: [ exclusions noneSatisfy: [ :each | each handlesAnnouncement: anAnnouncement ] ]
]

{ #category : #initialization }
AnnouncementSetWithExclusions >> initialize [

	super initialize.
	exclusions := OrderedCollection new
]

{ #category : #initialization }
AnnouncementSetWithExclusions >> initialize: aNumber [

	super initialize: aNumber.
	exclusions := OrderedCollection new
]
