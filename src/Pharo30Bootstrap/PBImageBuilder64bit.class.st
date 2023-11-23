"
I create pharo images for Spur VMs in 64bit format.
"
Class {
	#name : 'PBImageBuilder64bit',
	#superclass : 'PBAbstractImageBuilder',
	#category : 'Pharo30Bootstrap',
	#package : 'Pharo30Bootstrap'
}

{ #category : 'running' }
PBImageBuilder64bit >> createVMStubs [

	super createVMStubs.
	objectSpace backend smallFloatClass: (self createStubForClassNamed: #SmallFloat64).
]

{ #category : 'initialization' }
PBImageBuilder64bit >> initializeBackend [

	espellBackend := EPSimulatorBackend for64Bit forBootstrap
]
