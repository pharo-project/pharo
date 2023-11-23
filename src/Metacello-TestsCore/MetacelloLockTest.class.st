Class {
	#name : 'MetacelloLockTest',
	#superclass : 'TestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'accessing' }
MetacelloLockTest >> project [

	^ BaselineOfTestToLock1 project
]

{ #category : 'tests' }
MetacelloLockTest >> testLoadLockedWillNotIncludeLockedPackages [
	"Using record as a way to determine which packages will be loaded"

	self 
		assertCollection: (self project version record packages collect: #name)
		hasSameElements: #( 
			'Package1'
			'BaselineOfTestToLock2' 
			'TestToLockPackage1' 
			'TestToLockPackage2').
		
	[
		"baseline needs to be registered to be locked"
		Metacello new baseline: 'TestToLock2'; register.
		Metacello new baseline: 'TestToLock2'; lock.
		self 
			assertCollection: (self project version record packages collect: #name)
			hasSameElements: #('Package1') ]
	ensure: [ 
		Metacello new baseline: 'TestToLock2'; unlock. 
		Metacello new baseline: 'TestToLock2'; unregister ]
]

{ #category : 'tests' }
MetacelloLockTest >> testRegisterProject [

	self 
		denyCollection: (MetacelloProjectRegistration registry baselineProjectSpecs collect: #name)
		includesAny: #('TestToLock2').

	[
		Metacello new baseline: 'TestToLock2'; register.
		self 
			assertCollection: (MetacelloProjectRegistration registry baselineProjectSpecs collect: #name)
			includesAny: #('TestToLock2') ]
	ensure: [ 
		 Metacello new baseline: 'TestToLock2'; unregister ]
]
