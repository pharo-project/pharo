Extension { #name : #ClassAnnouncement }

{ #category : #'*Calypso-SystemQueries' }
ClassAnnouncement >> affectsClass: aClass [
	^self classAffected == aClass
]

{ #category : #'*Calypso-SystemQueries' }
ClassAnnouncement >> affectsClasses [
	^true
]

{ #category : #'*Calypso-SystemQueries' }
ClassAnnouncement >> affectsClassesDefinedInPackage: aPackage [
	^self packagesAffected includes: aPackage
]

{ #category : #'*Calypso-SystemQueries' }
ClassAnnouncement >> affectsClassesExtendedInPackage: aPackage [
	^aPackage extendsClass: self classAffected
]

{ #category : #'*Calypso-SystemQueries' }
ClassAnnouncement >> affectsPackages [
	^true
]
