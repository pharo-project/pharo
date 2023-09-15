"
SUnit tests for RPackage
"
Class {
	#name : #RPackageObsoleteTest,
	#superclass : #RPackageIncrementalTest,
	#instVars : [
		'notRun'
	],
	#category : #'RPackage-Tests'
}

{ #category : #accessing }
RPackageObsoleteTest >> setNotRun [
	notRun := true
]

{ #category : #tests }
RPackageObsoleteTest >> testAnnouncementClassRemovedIsRaisedOnRemoveFromSystem [

	| foo |
	[
	notRun := false.
	SystemAnnouncer uniqueInstance weak when: ClassRemoved send: #setNotRun to: self.
	foo := self newClassNamed: #FooForTest2 in: self p1Name.
	self deny: notRun.
	foo removeFromSystem.
	self assert: notRun ] ensure: [ SystemAnnouncer uniqueInstance unsubscribe: self ]
]

{ #category : #tests }
RPackageObsoleteTest >> testMethodPackageFromObsoleteClass [

	| pack method foo |
	pack := self organizer ensurePackage: 'P1'.
	foo := self newClassNamed: #FooForTest in: pack.
	foo compile: 'bar ^42'.
	method := foo >> #bar.

	foo obsolete.
	self assert: method package equals: foo package
]

{ #category : #tests }
RPackageObsoleteTest >> testMethodPackageOfRemovedClass [

	| pack method foo |
	pack := self organizer ensurePackage: 'P1'.
	foo := self newClassNamed: #FooForTest2 in: pack.
	foo compile: 'bar ^42'.
	method := foo >> #bar.
	foo removeFromSystem.
	self deny: (pack includesClassNamed: #FooForTest2)
]
