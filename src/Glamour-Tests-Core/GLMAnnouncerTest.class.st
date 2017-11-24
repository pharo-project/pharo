Class {
	#name : #GLMAnnouncerTest,
	#superclass : #TestCase,
	#instVars : [
		'announcer'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #running }
GLMAnnouncerTest >> setUp [
	announcer := GLMAnnouncer new
]

{ #category : #tests }
GLMAnnouncerTest >> testNestedSuspendAllWhile [
	| announcement |
	announcer
		when: GLMTestAnnouncement
		do: [ :ann | announcement := ann ].
	
	announcement := nil.
	announcer suspendAllWhile: [
		announcer suspendAllWhile: [ ].
		announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil
]

{ #category : #tests }
GLMAnnouncerTest >> testNestedSuspendParticularAnnouncementWhile [
	| announcement instance2 announcement2 |
	announcer
		when: GLMTestAnnouncement, GLMTestAnnouncement2
		do: [ :ann | announcement := ann ].
	
	announcement2 := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [
		announcer suspendAll: {GLMTestAnnouncement2} while: [  
			instance2 := announcer announce: GLMTestAnnouncement.
			announcement2 := announcement.
			announcement := nil. ].
		announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil.
	self assert: announcement2 equals: instance2.
]

{ #category : #tests }
GLMAnnouncerTest >> testSimpleNestedSuspendParticularAnnouncementWhile [
	| announcement |
	announcer
		when: GLMTestAnnouncement
		do: [ :ann | announcement := ann ].
	
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [
		announcer suspendAll: {} while: [ ].
		announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil
]

{ #category : #tests }
GLMAnnouncerTest >> testSuspendAllWhile [
	| announcement instance |
	announcer when: GLMTestAnnouncement do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: GLMTestAnnouncement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAllWhile: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil
]

{ #category : #tests }
GLMAnnouncerTest >> testSuspendParticularAnnouncementWhile [
	| announcement instance |
	announcer when: GLMTestAnnouncement do: [ :ann | announcement := ann ].
	announcer when: GLMTestAnnouncement2 do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: GLMTestAnnouncement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: Announcement ].
	self assert: announcement isNil.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: GLMTestAnnouncement2 ].
	self assert: announcement equals: instance.
]

{ #category : #tests }
GLMAnnouncerTest >> testSuspendParticularAnnouncementWithInheritanceWhile [
	| announcement instance |
	announcer when: Announcement do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: Announcement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAll: {Announcement} while: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil
]
