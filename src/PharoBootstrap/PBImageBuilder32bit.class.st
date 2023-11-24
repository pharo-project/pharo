"
I create pharo images for Spur VMs in 32bit format.
"
Class {
	#name : 'PBImageBuilder32bit',
	#superclass : 'PBAbstractImageBuilder',
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'initialization' }
PBImageBuilder32bit >> initializeBackend [

	espellBackend := EPSimulatorBackend for32Bit forBootstrap
]
