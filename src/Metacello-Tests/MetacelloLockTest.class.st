Class {
	#name : 'MetacelloLockTest',
	#superclass : 'TestCase',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'private' }
MetacelloLockTest >> performTest [

	MetacelloProjectRegistration useRegistry: MetacelloProjectRegistry new during: [ super performTest ]
]

{ #category : 'accessing' }
MetacelloLockTest >> project [

	^ BaselineOfTestToLock1 project
]

{ #category : 'tests' }
MetacelloLockTest >> testLoadLockedWillNotIncludeLockedPackages [
	"Using record as a way to determine which packages will be loaded"

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.
	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		lock.
	self assertCollection: ((self project version recordWithEngine: MetacelloScriptEngine new) packages collect: #name) hasSameElements: #( 'Package1' )
]

{ #category : 'tests' }
MetacelloLockTest >> testRegisterProject [

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.

	self assertCollection: (MetacelloProjectRegistration registry projectSpecs collect: #name) includesAny: #( 'TestToLock2' )
]
