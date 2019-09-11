Extension { #name : #MethodAnnouncement }

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsClassExtension [
	^self methodAffected isExtension
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsClassesExtendedInPackage: aPackage [
	^ self isProvidedByExtension and: [ self affectsMethodsDefinedInPackage: aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsMethod: aMethod [
	^self methodAffected == aMethod
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsMethods [
	^true
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self classAffected == aClass
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsMethodsDefinedInPackage: aPackage [

	^methodPackage == aPackage or: [ self methodAffected package == aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsMethodsTaggedWith: tagName [
	^method isTaggedWith: tagName asSymbol
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> affectsPackages [
	"any method makes package dirty"
	 
	^true
]

{ #category : #'*Calypso-SystemQueries' }
MethodAnnouncement >> canAffectResultOfMethodQuery: aMethodQuery [

	^aMethodQuery isAffectedByChangedMethod: self methodAffected
]
