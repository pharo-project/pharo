"
My subclasses are very simple factories that serve as entry point to create different kind of behaviors. There should be no instances of me.

The behaviors are composition of a RGBehavior instances and a strategy that describes a kind of the behavior. The behavior factory only create sach pairs so for example my subclass RGClass creates a composition of RGBehavior instance with a RGClassStrategy. 
"
Class {
	#name : #RGBehaviorFactory,
	#superclass : #Object,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGBehaviorFactory class >> named: aString [

	^ (RGBehavior named: aString behaviorStrategy: self newStrategy)
		yourself
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> named: aString parent: anRGObject [

	^ (RGBehavior named: aString parent: anRGObject  behaviorStrategy: (self newStrategyFor: anRGObject environment))
		yourself
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> newStrategy [

	self subclassResponsibility 
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> newStrategyFor: anRGEnvironment [

	^ self newStrategy
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> parent: anRGObject [

	^ (RGBehavior parent: anRGObject behaviorStrategy: (self newStrategyFor: anRGObject environment))
		yourself
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> unnamed [

	^ RGBehavior unnamedWithBehaviorStrategy: self newStrategy

]

{ #category : #'instance creation' }
RGBehaviorFactory class >> unresolved [

	^ RGBehavior unresolvedWithBehaviorStrategy: self newStrategy

]

{ #category : #'instance creation' }
RGBehaviorFactory class >> unresolvedNamed: aString withParent: anRGDefintion [

	^ RGBehavior unresolvedNamed: aString withParent: anRGDefintion behaviorStrategy: (self newStrategyFor: anRGDefintion environment)
]

{ #category : #'instance creation' }
RGBehaviorFactory class >> unresolvedWithParent: anRGDefintion [

	^ RGBehavior unresolvedWithParent: anRGDefintion behaviorStrategy: (self newStrategyFor: anRGDefintion environment) 
]
