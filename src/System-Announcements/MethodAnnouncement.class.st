"
An abstract class for method announcements.
"
Class {
	#name : #MethodAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'method',
		'methodPackage'
	],
	#category : #'System-Announcements-System-Methods'
}

{ #category : #testing }
MethodAnnouncement >> affectsClassExtension [
	^self methodAffected isExtension
]

{ #category : #testing }
MethodAnnouncement >> affectsClassesExtendedInPackage: aPackage [
	^ self isProvidedByExtension and: [ self affectsMethodsDefinedInPackage: aPackage ]
]

{ #category : #testing }
MethodAnnouncement >> affectsMethod: aMethod [
	^self methodAffected == aMethod
]

{ #category : #testing }
MethodAnnouncement >> affectsMethods [
	^true
]

{ #category : #testing }
MethodAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self classAffected == aClass
]

{ #category : #testing }
MethodAnnouncement >> affectsMethodsDefinedInPackage: aPackage [

	^methodPackage == aPackage or: [ self methodAffected package == aPackage ]
]

{ #category : #testing }
MethodAnnouncement >> affectsMethodsTaggedWith: tagName [
	^method isTaggedWith: tagName asSymbol
]

{ #category : #testing }
MethodAnnouncement >> affectsPackages [
	"any method makes package dirty"
	 
	^true
]

{ #category : #accessing }
MethodAnnouncement >> classAffected [
	^ self methodOrigin
]

{ #category : #testing }
MethodAnnouncement >> isProvidedByATrait [

	^ self methodOrigin ~= self methodClass

]

{ #category : #testing }
MethodAnnouncement >> isProvidedByExtension [
	^self methodOrigin package ~= self methodPackage
]

{ #category : #accessing }
MethodAnnouncement >> method [
	
	^ method
]

{ #category : #accessing }
MethodAnnouncement >> method: aCompiledMethod [
	method := aCompiledMethod.
	methodPackage := aCompiledMethod package
]

{ #category : #accessing }
MethodAnnouncement >> methodAffected [
	^self method
]

{ #category : #accessing }
MethodAnnouncement >> methodClass [
	^ method methodClass
]

{ #category : #accessing }
MethodAnnouncement >> methodOrigin [
	^ method origin
]

{ #category : #accessing }
MethodAnnouncement >> methodPackage [
	^ methodPackage ifNil: [ methodPackage := method package ]
]

{ #category : #accessing }
MethodAnnouncement >> packagesAffected [
	^{self methodPackage}
]

{ #category : #accessing }
MethodAnnouncement >> protocol [
	
	^ method protocol
]

{ #category : #accessing }
MethodAnnouncement >> protocolsAffected [
	^{self protocol}
]

{ #category : #accessing }
MethodAnnouncement >> selector [
	
	^ method selector
]
