"
This announcement corresponds to the ReorganizedEvent, which seems to (by looking at the references of ReorganizedEvent) be raised when:
- we rename a protocol (see 'renameCategory:toBe: '). if The category is not empty, SystemMethodRecategorizedAnnouncement will also be emitted
- we sort (modify the order of) protocols (see 'sortCategories')
- we add a protocol (see 'addCategory:before:')
- we remove a protocol (see 'removeCategory:' 'removeEmptyCategories')
"
Class {
	#name : #ClassReorganized,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'classReorganized'
	],
	#category : 'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassReorganized class >> class: aClass [
	^self new
			classReorganized: aClass;
			yourself
]

{ #category : #accessing }
ClassReorganized >> classAffected [
	^self classReorganized
]

{ #category : #accessing }
ClassReorganized >> classReorganized [
	
	^ classReorganized
]

{ #category : #accessing }
ClassReorganized >> classReorganized: anObject [
	
	classReorganized := anObject
]
