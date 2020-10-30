Extension { #name : #Announcement }

{ #category : #'*System-Announcements' }
Announcement >> affectsClass: aClass [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsClassExtension [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsClasses [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsClassesDefinedInPackage: aPackage [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsClassesExtendedInPackage: aPackage [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethod: aMethod [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethodTagIn: aClass [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethods [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethodsDefinedInClass: aClass [
	^ false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethodsDefinedInPackage: aPackage [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsMethodsTaggedWith: tagName [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsPackage: aPackage [
	^aPackage includesClassesAffectedBy: self

]

{ #category : #'*System-Announcements' }
Announcement >> affectsPackages [
	^false
]

{ #category : #'*System-Announcements' }
Announcement >> affectsVariablesOf: aClass [
	^false
]
