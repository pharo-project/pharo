"
I represent a change in a class or a trait.
"
Class {
	#name : #EpBehaviorChange,
	#superclass : #EpCodeChange,
	#category : #'Epicea-Model'
}

{ #category : #comparing }
EpBehaviorChange >> = other [

	^ self class = other class and: [ self behaviorAffected = other behaviorAffected ]
]

{ #category : #accessing }
EpBehaviorChange >> affectedPackageName [
	
	^  self behaviorAffected package
]

{ #category : #accessing }
EpBehaviorChange >> behaviorAffected [
	^ self shouldBeImplemented 
]

{ #category : #accessing }
EpBehaviorChange >> behaviorAffectedName [
	^ self shouldBeImplemented 
]

{ #category : #testing }
EpBehaviorChange >> doesOverride: aCodeChange [ 
	
	^ aCodeChange class = self class and: [ 
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]
]

{ #category : #comparing }
EpBehaviorChange >> hash [

	^ self class hash bitXor: self behaviorAffected hash
]

{ #category : #testing }
EpBehaviorChange >> isEpBehaviorChange [
	^true
]

{ #category : #printing }
EpBehaviorChange >> printOn: aStream [

	super printOn: aStream.

	aStream 
		nextPut: $(;
		nextPutAll: self behaviorAffectedName;
		nextPut: $)
]
