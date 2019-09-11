"
A factory that produces instances of RGBehavior composed with RGTraitStrategy
"
Class {
	#name : #RGTrait,
	#superclass : #RGBehaviorFactory,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGTrait class >> newStrategy [

	^ RGTraitStrategy basicNew
]

{ #category : #'instance creation' }
RGTrait class >> newStrategyFor: anRGEnvironment [

	^ (self strategyClassForVersion: anRGEnvironment version) basicNew

]

{ #category : #'instance creation' }
RGTrait class >> strategyClassForVersion: aVersionNumber [

	^ (aVersionNumber <= 6)
		ifTrue: [ RGTraitStrategy ]
		ifFalse: [ RGTraitV2Strategy ]

]
