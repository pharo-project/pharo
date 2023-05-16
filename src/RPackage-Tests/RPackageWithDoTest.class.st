"
SUnit tests for RPackage
"
Class {
	#name : #RPackageWithDoTest,
	#superclass : #TestCase,
	#instVars : [
		'x'
	],
	#category : #'RPackage-Tests'
}

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> do: aBlock [

	x := 5.
	[aBlock value] ensure: [x :=3.]
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> doC: aBlock [

	x := 5.
	[aBlock value] ifCurtailed: [x :=3]
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> doOnDo: aBlock [

	x := 5.
	[aBlock value.
	self ensure] on: Error do:  [:ex | self ensure]
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> ensure [

	x := 3
]

{ #category : #accessing }
RPackageWithDoTest >> organizerAnnouncer [
	^ SystemAnnouncer uniqueInstance private
]

{ #category : #tests }
RPackageWithDoTest >> packageClass [
	^ RPackage
]

{ #category : #tests }
RPackageWithDoTest >> packageOrganizerClass [
	^ RPackageOrganizer
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> testDo [
	[ self do: [ Error signal ] ]
		on: Error
		do: [ :ex |  ].
	"necessary else SUnit believes that the error is not handled"
	self assert: x equals: 3
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> testDo2 [
	[ self do: [ self error ] ]
		on: Error
		do: [ :ex |  ].
	"necessary else SUnit believes that the error is not handled"
	self assert: x equals: 3
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> testDoC [
	[ self doC: [ self error ] ]
		on: Error
		do: [ :e |  ].
	"necessary else SUnit believes that the error is not handled"
	self assert: x equals: 3
]

{ #category : #'tests - simple ensuring' }
RPackageWithDoTest >> testOnDo [
	self doOnDo: [ self error ].
	self assert: x equals: 3.
	x := 77.
	self doOnDo: [ x = 21 ].
	self assert: x equals: 3
]

{ #category : #tests }
RPackageWithDoTest >> testOrganizerContainsPackages [

	self
		assert: Smalltalk organization packageNames isNotEmpty
		description: 'RPackageOrganizer should contains package names since an image should at least contains one package.'.
	self
		assert: Smalltalk organization packages isNotEmpty
		description: 'RPackageOrganizer should not be empty since an image should at least contains one package.'
]

{ #category : #tests }
RPackageWithDoTest >> testWithDoIsCorrectlyReinstallingDefault [

	| current empty |
	current := Smalltalk organization.
	empty := self packageOrganizerClass basicNew initialize.
	empty debuggingName: 'empty from PackageWithDoTest'.
	self packageClass withOrganizer: empty do: [
		self
			assert: (self organizerAnnouncer hasSubscriber: empty)
			description: 'During #withOrganizer:do:, the system announcer should have the organizer as parameter in its subscriber.'.
		self
			deny: (self organizerAnnouncer hasSubscriber: current)
			description: 'During #withOrganizer:do:, the system announcer shouldn''t have the default organizer in its subscriber.' ].
	self
		assert: (self organizerAnnouncer hasSubscriber: current)
		description: 'After the use of #withOrganizer:do:, the default organizer should be reinstalled in system announcer subscribers.'.
	self
		deny: (self organizerAnnouncer hasSubscriber: empty)
		description: 'After the use of #withOrganizer:do:, the empty organizer should be removed from the system announcer subscribers.'
]

{ #category : #tests }
RPackageWithDoTest >> testWithDoIsCorrectlyReinstallingDefaultEvenIfHalt [

	| current empty |
	current := Smalltalk organization.
	empty := self packageOrganizerClass basicNew initialize.
	[
	self packageClass withOrganizer: empty do: [
		self
			assert: (self organizerAnnouncer hasSubscriber: empty)
			description: 'During #withOrganizer:do:, the system announcer should have the organizer as parameter in its subscriber.'.
		self
			deny: (self organizerAnnouncer hasSubscriber: current)
			description: 'During #withOrganizer:do:, the system announcer shouldn''t have the default organizer in its subscriber.'.
		self error ] ]
		on: Error
		do: [ :ex |  ].
	self
		assert: (self organizerAnnouncer hasSubscriber: current)
		description: 'After the use of #withOrganizer:do:, the default organizer should be reinstalled in system announcer subscribers.'.
	self
		deny: (self organizerAnnouncer hasSubscriber: empty)
		description: 'After the use of #withOrganizer:do:, the empty organizer should be removed from the system announcer subscribers.'
]
