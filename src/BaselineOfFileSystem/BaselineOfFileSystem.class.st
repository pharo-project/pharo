Class {
	#name : 'BaselineOfFileSystem',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFileSystem',
	#package : 'BaselineOfFileSystem'
}

{ #category : 'accessing' }
BaselineOfFileSystem class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'accessing' }
BaselineOfFileSystem class >> testsPackageNames [

	^ self packagesOfGroupNamed: #Tests
]

{ #category : 'baselines' }
BaselineOfFileSystem >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'FileSystem-Path';
				package: 'FileSystem-Core';
				package: 'FileSystem-Disk';
				
				package: 'FileSystem-Core-Tests';
				package: 'FileSystem-Disk-Tests';
				package: 'FileSystem-Tests-Attributes'.

			spec
				group: 'Core' with: { 'FileSystem-Path'. 'FileSystem-Core'. 'FileSystem-Disk' };
				group: 'Tests' with: { 'FileSystem-Core-Tests'. 'FileSystem-Disk-Tests'. 'FileSystem-Tests-Attributes' } ]
]
