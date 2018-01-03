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

{ #category : #accessing }
RPackageWithDoTest >> announcer [
	^ SystemAnnouncer uniqueInstance private
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> do: aBlock [ 

	x := 5.
	[aBlock value] ensure: [x :=3.]
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> doC: aBlock [ 
	
	x := 5.
	[aBlock value] ifCurtailed: [x :=3]
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> doOnDo: aBlock [ 
	
	x := 5.
	[aBlock value.
	self ensure] on: Error do:  [:ex | self ensure].
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> ensure [

	x := 3
]

{ #category : #tests }
RPackageWithDoTest >> info [
	
	
	self announcer hasSubscriber: RPackageOrganizer default.
	Smalltalk garbageCollect.
	RPackageOrganizer allInstances collect: [:each | self announcer hasSubscriber: each ] 
]

{ #category : #tests }
RPackageWithDoTest >> packageClass [
	^ RPackage
]

{ #category : #tests }
RPackageWithDoTest >> packageOrganizerClass [
	^ RPackageOrganizer
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> testDo [
	"self run: #testDo"
	
	[self do: [ Error signal]] on:Error do: [:ex|].
	"necessary else SUnit believe that the error is not handle"
	self assert: x = 3

]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> testDo2 [
	"self run: #testDo2"
	
	[self do: [ self error]
		] on: Error do: [:ex | ].
	self assert: x = 3

]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> testDoC [
	"self run: #testDoC"
	
	[self doC: [ self error]] on: Error do: [:e|].
	"necessary for SUnit"
	self assert: x = 3

]

{ #category : #tests }
RPackageWithDoTest >> testInvariant [
	"The default invariant"
	"self run: #testInvariant"
	
	self assert: (self packageOrganizerClass default packageNames size > 50).
	"to be sure that this is real default one"
	self assert: (self announcer hasSubscriber: self packageOrganizerClass default).
	"note that this test is not precise enough because I could get the wrong one."
	Smalltalk garbageCollect.
	Smalltalk garbageCollect.
	self assert: self packageOrganizerClass allInstances size equals: 1.
	
	self packageOrganizerClass allInstances collect: [:each | self announcer hasSubscriber: each ] 
	"To debug..."
]

{ #category : #'simple ensure tests' }
RPackageWithDoTest >> testOnDo [
	"self run: #testOnDo"
	
	self doOnDo: [ self error].
	self assert: x = 3.
	x := 77.
	self doOnDo: [x = 21].
	self assert: x = 3

]

{ #category : #tests }
RPackageWithDoTest >> testWithDoIsCorrectlyReinstallingDefault [
	
	| current empty |
	current := self packageOrganizerClass default.  
	empty := self packageOrganizerClass basicNew initialize.
	empty debuggingName: 'empty from PackageWithDoTest'.
	self packageClass 
		withOrganizer: empty
		do: [ 
			self assert: (self announcer hasSubscriber: empty).
			self deny: (self announcer hasSubscriber: current)].
	self assert: (self announcer hasSubscriber: current).
	self deny: (self announcer hasSubscriber: empty)
]

{ #category : #tests }
RPackageWithDoTest >> testWithDoIsCorrectlyReinstallingDefaultEvenIfHalt [

	| current empty |
	current := self packageOrganizerClass default.  
	empty := self packageOrganizerClass basicNew initialize.
	[ self packageClass 
		withOrganizer: empty
		do: [ 		
			self assert: (self announcer hasSubscriber: empty).
			self deny: (self announcer hasSubscriber: current).
			self error ] ]
		on: Error do: [:ex|].
	
	self assert: (self announcer hasSubscriber: current).
	self deny: (self announcer hasSubscriber: empty)
		
		



]
