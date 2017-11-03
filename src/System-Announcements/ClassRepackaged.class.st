"
Notify about repackaging of a class
"
Class {
	#name : #ClassRepackaged,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'classRepackaged',
		'newPackage',
		'oldPackage'
	],
	#category : #System-Announcements-System-Packages
}

{ #category : #'instance creation' }
ClassRepackaged class >> classRepackaged: aClass oldPackage: oldPackage newPackage: newPackage [
	^self new
			classRepackaged: aClass;
			oldPackage: oldPackage;
			newPackage: newPackage;
			yourself.
]

{ #category : #accessing }
ClassRepackaged >> classAffected [
	
	^ classRepackaged
]

{ #category : #accessing }
ClassRepackaged >> classRepackaged [
	
	^ classRepackaged
]

{ #category : #accessing }
ClassRepackaged >> classRepackaged: anObject [
	
	classRepackaged := anObject
]

{ #category : #accessing }
ClassRepackaged >> newPackage [
	
	^ newPackage
]

{ #category : #accessing }
ClassRepackaged >> newPackage: anObject [
	
	newPackage := anObject
]

{ #category : #accessing }
ClassRepackaged >> oldPackage [
	
	^ oldPackage
]

{ #category : #accessing }
ClassRepackaged >> oldPackage: anObject [
	
	oldPackage := anObject
]

{ #category : #accessing }
ClassRepackaged >> packagesAffected [
	^{oldPackage. newPackage}
]
