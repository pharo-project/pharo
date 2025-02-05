Class {
	#name : 'MCPackageLoaderTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'loader'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MCPackageLoaderTest >> setUp [
	super setUp.
	loader := MCPackageLoader new.
]

{ #category : 'tests' }
MCPackageLoaderTest >> testCanLoadSnapshotInAnotherEnvironment [

	| env package |
	env := SystemEnvironment new.
	env at: #MCSnapshotTest put: MCSnapshotTest. 
	loader installSnapshot: self mockSnapshot.
	loader targetEnvironment: env.
	
	loader load.
	
	package := self packageOrganizer packageNamed: MCSnapshotResource mockPackageName.
	
	self assert: (env classNames includesAll: package definedClassNames).
	self assert: ((env allMethods collect: #name) includesAll: (package methods collect: #name)).

]
