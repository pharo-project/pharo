"
I create pharo images for Spur VMs in 32bit format.
"
Class {
	#name : #PBImageBuilderSpur5032bit,
	#superclass : #PBImageBuilderSpur50,
	#category : 'Pharo30Bootstrap'
}

{ #category : #initialization }
PBImageBuilderSpur5032bit >> initializeBackend [

	espellBackend := EPSimulatorBackend for32Bit forBootstrap
]
