"
I'm represent an entry in the spaceTally.
"
Class {
	#name : #SpaceTallyItem,
	#superclass : #Object,
	#instVars : [
		'analyzedClassName',
		'codeSize',
		'instanceCount',
		'spaceForInstances'
	],
	#category : #'Tool-Profilers'
}

{ #category : #'instance creation' }
SpaceTallyItem class >> analyzedClassName: aClassName [

	^ self new
		analyzedClassName: aClassName ; yourself
		
]

{ #category : #'instance creation' }
SpaceTallyItem class >> analyzedClassName: aClassName codeSize: codeSize instanceCount: instanceCount spaceForInstances: spaceForInstances [

	^ self new
		analyzedClassName: aClassName ;
		codeSize: codeSize ;
		instanceCount: instanceCount ;
		spaceForInstances: spaceForInstances ; yourself
]

{ #category : #accessing }
SpaceTallyItem >> analyzedClassName [

	^ analyzedClassName
]

{ #category : #accessing }
SpaceTallyItem >> analyzedClassName: aClassName [

	analyzedClassName := aClassName
]

{ #category : #accessing }
SpaceTallyItem >> codeSize [

	^ codeSize
]

{ #category : #accessing }
SpaceTallyItem >> codeSize: aNumber [

	codeSize := aNumber
]

{ #category : #accessing }
SpaceTallyItem >> instanceCount [

	^ instanceCount
]

{ #category : #accessing }
SpaceTallyItem >> instanceCount: aNumber [

	instanceCount := aNumber
]

{ #category : #printing }
SpaceTallyItem >> printOn: aStream [

	analyzedClassName isNil
		ifFalse: [aStream nextPutAll: analyzedClassName asString]. 
	aStream nextPutAll: ' ('.
	codeSize isNil
		ifFalse: [aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize asString]. 
	instanceCount isNil
		ifFalse: [aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount asString]. 
	spaceForInstances isNil
		ifFalse: [aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances asString]. 
	aStream nextPut: $).
	
]

{ #category : #accessing }
SpaceTallyItem >> spaceForInstances [

	^ spaceForInstances
]

{ #category : #accessing }
SpaceTallyItem >> spaceForInstances: aNumber [

	spaceForInstances := aNumber
]
