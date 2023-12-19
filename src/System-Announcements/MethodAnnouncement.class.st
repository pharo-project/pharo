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
