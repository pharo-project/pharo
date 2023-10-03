Class {
	#name : 'SycRemoveNewPackageCommandTest',
	#superclass : 'TestCase',
	#instVars : [
		'mockPackage'
	],
	#category : 'SystemCommands-PackageCommands-Tests',
	#package : 'SystemCommands-PackageCommands-Tests'
}

{ #category : 'accessing' }
SycRemoveNewPackageCommandTest >> mockPackage [

	^ mockPackage
]

{ #category : 'accessing' }
SycRemoveNewPackageCommandTest >> mockPackage: anObject [

	mockPackage := anObject
]

{ #category : 'running' }
SycRemoveNewPackageCommandTest >> packageNameForTesting [

	^ 'Test-Package'
]

{ #category : 'running' }
SycRemoveNewPackageCommandTest >> setUp [

	super setUp.
	mockPackage := MockObject new on: #removeFromSystem

]

{ #category : 'tests' }
SycRemoveNewPackageCommandTest >> testExecute [

	| command |

	command := SycRemovePackageCommand new
		packages: { mockPackage };
		execute.

	self assert: command packages isEmpty.
	self verify: mockPackage

]
