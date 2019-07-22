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
	#category : #'Tool-Profilers-Space'
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

	analyzedClassName ifNotNil: [ aStream print: analyzedClassName ].
	aStream nextPutAll: ' ('.
	codeSize
		ifNotNil: [ aStream
				nextPutAll: 'code size: ';
				print: codeSize
			].
	instanceCount
		ifNotNil: [ aStream
				nextPutAll: ' instance count: ';
				print: instanceCount
			].
	spaceForInstances
		ifNotNil: [ aStream
				nextPutAll: ' space for instances: ';
				print: spaceForInstances
			].
	aStream nextPut: $)
]

{ #category : #accessing }
SpaceTallyItem >> spaceForInstances [

	^ spaceForInstances
]

{ #category : #accessing }
SpaceTallyItem >> spaceForInstances: aNumber [

	spaceForInstances := aNumber
]
