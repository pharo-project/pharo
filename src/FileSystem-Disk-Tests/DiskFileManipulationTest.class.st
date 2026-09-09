Class {
	#name : 'DiskFileManipulationTest',
	#superclass : 'FileManipulationTest',
	#category : 'FileSystem-Disk-Tests-Base',
	#package : 'FileSystem-Disk-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
DiskFileManipulationTest >> root [

	^ FileLocator temp
]
