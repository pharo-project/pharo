"
A PragmaCollectorReset is an announce which is announced by a PragamColllector when it is reset.
see PragmaCollector>>reset.

Instance Variables
	collector:		<PragmaCollector>

collector
	- the collector which is reset

"
Class {
	#name : #PragmaCollectorReset,
	#superclass : #Announcement,
	#instVars : [
		'collector'
	],
	#category : #'PragmaCollector-Announcements'
}

{ #category : #'instance creation' }
PragmaCollectorReset class >> collector: aCollector [
	^ self new collector: aCollector
]

{ #category : #accessing }
PragmaCollectorReset >> collector [
	^collector
]

{ #category : #accessing }
PragmaCollectorReset >> collector: aCollector [
	collector := aCollector
]
