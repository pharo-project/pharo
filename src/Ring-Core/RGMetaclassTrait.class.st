"
A factory that produces instances of RG2Behavior composed with RGMetaclassTraitStrategy
"
Class {
	#name : #RGMetaclassTrait,
	#superclass : #RGBehaviorFactory,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGMetaclassTrait class >> newStrategy [

	^ RGMetaclassTraitStrategy unresolved
]

{ #category : #'instance creation' }
RGMetaclassTrait class >> newStrategyFor: anRGEnvironment [

	^ (self strategyClassForVersion: anRGEnvironment version) unresolved

]

{ #category : #'instance creation' }
RGMetaclassTrait class >> strategyClassForVersion: aVersionNumber [

	^ (aVersionNumber <= 6)
		ifTrue: [ RGMetaclassTraitStrategy ]
		ifFalse: [ RGMetaclassTraitV2Strategy ]

]
