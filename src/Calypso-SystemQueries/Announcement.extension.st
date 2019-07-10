Extension { #name : #Announcement }

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsClass: aClass [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsClassExtension [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsClasses [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsClassesDefinedInPackage: aPackage [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsClassesExtendedInPackage: aPackage [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethod: aMethod [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethodTagIn: aClass [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethods [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethodsDefinedInClass: aClass [
	^ false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethodsDefinedInPackage: aPackage [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsMethodsTaggedWith: tagName [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsPackage: aPackage [
	^aPackage includesClassesAffectedBy: self

]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsPackages [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> affectsVariablesOf: aClass [
	^false
]

{ #category : #'*Calypso-SystemQueries' }
Announcement >> canAffectResultOfMethodQuery: aMethodQuery [
	^false
]
