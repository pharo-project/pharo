"
I represent a change in the method dictionary of a class or a trait.
"
Class {
	#name : #EpMethodChange,
	#superclass : #EpCodeChange,
	#category : #Epicea-Model
}

{ #category : #accessing }
EpMethodChange >> behaviorAffectedName [

	^ self methodAffected className
]

{ #category : #testing }
EpMethodChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpMethodChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName and: [ 
		aCodeChange methodAffectedSelector = self methodAffectedSelector ] ]
]

{ #category : #testing }
EpMethodChange >> isEpMethodChange [

	^ true
]

{ #category : #accessing }
EpMethodChange >> methodAffected [
	
	^ self subclassResponsibility 
]

{ #category : #accessing }
EpMethodChange >> methodAffectedProtocol [

	^ self methodAffected protocol
]

{ #category : #accessing }
EpMethodChange >> methodAffectedSelector [

	^ self methodAffected selector
]

{ #category : #accessing }
EpMethodChange >> methodAffectedSourceCode [

	^ self methodAffected sourceCode
]

{ #category : #printing }
EpMethodChange >> printOn: aStream [

	super printOn: aStream.

	aStream 
		nextPut: $(;
		nextPutAll: self behaviorAffectedName;
		nextPutAll: '>>';
		nextPutAll: self methodAffectedSelector;
		nextPut: $)
]
