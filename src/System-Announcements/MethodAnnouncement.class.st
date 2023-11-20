"
An abstract class for method announcements.
"
Class {
	#name : 'MethodAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'method'
	],
	#category : 'System-Announcements-System-Methods',
	#package : 'System-Announcements',
	#tag : 'System-Methods'
}

{ #category : 'testing' }
MethodAnnouncement >> affectsClassExtension [
	^self methodAffected isExtension
]

{ #category : 'testing' }
MethodAnnouncement >> affectsClassesExtendedInPackage: aPackage [
	^ self isProvidedByExtension and: [ self affectsMethodsDefinedInPackage: aPackage ]
]

{ #category : 'testing' }
MethodAnnouncement >> affectsMethod: aMethod [
	^self methodAffected == aMethod
]

{ #category : 'testing' }
MethodAnnouncement >> affectsMethods [
	^true
]

{ #category : 'testing' }
MethodAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self classAffected == aClass
]

{ #category : 'testing' }
MethodAnnouncement >> affectsMethodsDefinedInPackage: aPackage [

	^ self methodPackage == aPackage or: [ self methodAffected package == aPackage ]
]

{ #category : 'testing' }
MethodAnnouncement >> affectsMethodsInProtocol: protocol [

	^ method protocolName == protocol
]

{ #category : 'testing' }
MethodAnnouncement >> affectsPackages [
	"any method makes package dirty"

	^true
]

{ #category : 'accessing' }
MethodAnnouncement >> classAffected [
	^ self methodOrigin
]

{ #category : 'testing' }
MethodAnnouncement >> isProvidedByExtension [
	^self methodOrigin package ~= self methodPackage
]

{ #category : 'accessing' }
MethodAnnouncement >> method [

	^ method
]

{ #category : 'accessing' }
MethodAnnouncement >> method: aCompiledMethod [

	method := aCompiledMethod
]

{ #category : 'accessing' }
MethodAnnouncement >> methodAffected [
	^self method
]

{ #category : 'accessing' }
MethodAnnouncement >> methodClass [
	^ method methodClass
]

{ #category : 'accessing' }
MethodAnnouncement >> methodOrigin [
	^ method origin
]

{ #category : 'accessing' }
MethodAnnouncement >> methodPackage [

	^ method package
]

{ #category : 'accessing' }
MethodAnnouncement >> packagesAffected [
	^{self methodPackage}
]

{ #category : 'accessing' }
MethodAnnouncement >> protocol [

	^ method protocol
]

{ #category : 'accessing' }
MethodAnnouncement >> protocolsAffected [
	^{self protocol}
]

{ #category : 'accessing' }
MethodAnnouncement >> selector [

	^ method selector
]
