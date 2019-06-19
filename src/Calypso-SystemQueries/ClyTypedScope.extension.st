Extension { #name : #ClyTypedScope }

{ #category : #'*Calypso-SystemQueries' }
ClyTypedScope >> canDetectAffectOnClassesBy: aSystemAnnouncement [
	"it is special hook to allow particular scopes define that all items should be checked.
	For example class hierarchy scope could not track that removed class was in hierarchy
	using only events. It should be improved in system but right now it is like that"		
	
	^true
]

{ #category : #'*Calypso-SystemQueries' }
ClyTypedScope >> includesClassGroupsAffectedBy: aSystemAnnouncement [

	^false
]

{ #category : #'*Calypso-SystemQueries' }
ClyTypedScope >> includesClassesAffectedBy: aSystemAnnouncement [

	^basisObjects anySatisfy: [ :each | 
		each includesClassesAffectedBy: aSystemAnnouncement ]
]

{ #category : #'*Calypso-SystemQueries' }
ClyTypedScope >> includesMethodsAffectedBy: aSystemAnnouncement [

	^basisObjects anySatisfy: [ :each | 
		each includesMethodsAffectedBy: aSystemAnnouncement ]
]

{ #category : #'*Calypso-SystemQueries' }
ClyTypedScope >> includesVariablesAffectedBy: aSystemAnnouncement [
	^false
]
