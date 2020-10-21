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

{ #category : #testing }
MethodRepackaged >> affectsClassExtension [
	^true
]

{ #category : #testing }
MethodRepackaged >> affectsClassesExtendedInPackage: aPackage [
	^(super affectsClassesExtendedInPackage: aPackage)
		or: [ oldPackage = aPackage ]
]

{ #category : #testing }
MethodRepackaged >> affectsMethodsDefinedInPackage: aPackage [
	^(super affectsMethodsDefinedInPackage: aPackage)
		or: [ oldPackage = aPackage ]
]

{ #category : #testing }
MethodRepackaged >> affectsPackage: aPackage [
	^newPackage == aPackage or: [ oldPackage == aPackage ]
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
