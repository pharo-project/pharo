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
	[ notRun := false.
	  SystemAnnouncer uniqueInstance 
		when: ClassRemoved send: #setNotRun to: self.
	foo := self createNewClassNamed: #FooForTest2.
	self deny: notRun.
	foo removeFromSystem.
	self assert: notRun ]
ensure: [ SystemAnnouncer uniqueInstance unsubscribe: self ]. 
]

{ #category : #tests }
RPackageObsoleteTest >> testMethodPackageFromObsoleteClass [
	
	| pack method foo | 
	[
	pack := self createNewPackageNamed: 'P1'.
	foo := self createNewClassNamed: #FooForTest inPackage: pack.
	foo compile: 'bar ^42'.
	method := foo>>#bar.

	foo obsolete.
	self assert: (method packageFromOrganizer: pack organizer) equals: foo package.
	] ensure: [ 
		foo ifNotNil: [ 
			foo setName: foo originalName.
			foo removeFromSystem.
			].
		testingEnvironment removeKey: #FooForTest  ifAbsent:  [].
	]
]

{ #category : #tests }
RPackageObsoleteTest >> testMethodPackageOfRemovedClass [
	
	| pack method foo | 
	pack := self createNewPackageNamed: 'P1'.
	foo := self createNewClassNamed: #FooForTest2 inPackage: pack.
	foo compileSilently: 'bar ^42'.
	method := foo>>#bar.
	foo removeFromSystem.
	self deny: (pack includesClassNamed: #FooForTest2 )
	
	
]
