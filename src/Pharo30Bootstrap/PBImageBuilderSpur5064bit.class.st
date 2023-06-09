"
I create pharo images for Spur VMs in 64bit format.
"
Class {
	#name : #PBImageBuilderSpur5064bit,
	#superclass : #PBImageBuilderSpur50,
	#category : 'Pharo30Bootstrap'
}

{ #category : #running }
PBImageBuilderSpur5064bit >> createVMStubs [

	super createVMStubs.
	objectSpace backend smallFloatClass: (self createStubForClassNamed: #SmallFloat64).
]

{ #category : #initialization }
PBImageBuilderSpur5064bit >> initializeBackend [

	espellBackend := EPSimulatorBackend for64Bit forBootstrap
]
