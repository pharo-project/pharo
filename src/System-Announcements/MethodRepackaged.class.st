"
Notify about repackaging of a method
"
Class {
	#name : #MethodRepackaged,
	#superclass : #MethodAnnouncement,
	#instVars : [
		'newPackage',
		'oldPackage'
	],
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'instance creation' }
MethodRepackaged class >> methodRepackaged: aMethod oldPackage: oldPackage newPackage: newPackage [
	^self new
			method: aMethod;
			oldPackage: oldPackage;
			newPackage: newPackage;
			yourself.
]

{ #category : #accessing }
MethodRepackaged >> methodRepackaged [
	
	^ method
]

{ #category : #accessing }
MethodRepackaged >> newPackage [
	
	^ newPackage
]

{ #category : #accessing }
MethodRepackaged >> newPackage: anObject [
	
	newPackage := anObject
]

{ #category : #accessing }
MethodRepackaged >> oldPackage [
	
	^ oldPackage
]

{ #category : #accessing }
MethodRepackaged >> oldPackage: anObject [
	
	oldPackage := anObject
]

{ #category : #accessing }
MethodRepackaged >> packagesAffected [
	^{oldPackage. newPackage}
]
