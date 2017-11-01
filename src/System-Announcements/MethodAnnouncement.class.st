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
	#category : 'System-Announcements-System-Methods'
}

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
