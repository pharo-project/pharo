Class {
	#name : 'MetacelloLockTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'accessing' }
MetacelloLockTest >> project [

	^ BaselineOfTestToLock1 project
]

{ #category : 'tests' }
MetacelloLockTest >> testLoadLockedWillNotIncludeLockedPackages [
	"Using record as a way to determine which packages will be loaded"
	| packages |
	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.
	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		lock.
	packages := OrderedCollection new.
	(Metacello new
		 baseline: 'TestToLock1';
		 repository: 'http://repo.com';
		 record) packageDirectivesDo: [ :each | packages add: each spec name ].
	self assertCollection: packages hasSameElements: #( 'Package1' )
]

{ #category : 'tests' }
MetacelloLockTest >> testRegisterProject [

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.

	self assertCollection: (MetacelloProjectRegistry projects collect: #name) includesAny: #( 'TestToLock2' )
]
