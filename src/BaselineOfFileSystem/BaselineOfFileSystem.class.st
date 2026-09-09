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
				package: 'FFI-Kernel';
				package: 'FileSystem-Path';
				package: 'FileSystem-Core' with: [ spec requires: #('FileSystem-Path')] ;
				package: 'FileSystem-Disk' with: [ spec requires: #('FileSystem-Core' 'FileSystem-Path')];
				package: 'FileSystem-Memory' with: [ spec requires: #('FileSystem-Core')];
				package: 'System-Sources-Files';
				
				package: 'FileSystem-Core-Tests' with: [ spec requires: #('FileSystem-Core' 'FileSystem-Disk' 'FileSystem-Path')];
				package: 'FileSystem-Disk-Tests' with: [ spec requires: #('FileSystem-Core' 'FileSystem-Disk' 'FileSystem-Core-Tests')];
				package: 'FileSystem-Memory-Tests' with: [ spec requires: #('FileSystem-Core' 'FileSystem-Memory' 'FileSystem-Core-Tests')];
				package: 'FileSystem-Tests-Attributes' with: [ spec requires: #('FileSystem-Core')];
				package: 'FileSystem-PlatformSpecific-Tests' with: [ spec requires: #('FileSystem-Core')].

			spec
				group: 'Core' with: {'FFI-Kernel'. 'FileSystem-Path'. 'FileSystem-Core'. 'FileSystem-Disk'. 'System-Sources-Files'. 'FileSystem-Memory'};
				"Basic Tests"
				group: 'Tests' with: { 'FileSystem-Core-Tests'. 'FileSystem-Disk-Tests'. 'FileSystem-Tests-Attributes' };
				"Tests depending on LibC/UFFI"
				group: 'TestsFull' with: {'Tests'. 'FileSystem-PlatformSpecific-Tests' .'FileSystem-Memory-Tests'} ]
]
