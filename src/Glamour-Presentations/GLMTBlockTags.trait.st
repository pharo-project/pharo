"
I provide an api for computing tags.
"
Trait {
	#name : #GLMTBlockTags,
	#category : 'Glamour-Presentations'
}

{ #category : #tags }
GLMTBlockTags >> computeTagsFilterFor: aGlamourValue [ 

	^ self computeTagsFor: aGlamourValue from: self tagsFilterBlock
]

{ #category : #tags }
GLMTBlockTags >> computeTagsFor: aGlamourValue [ 

	^ self computeTagsFor: aGlamourValue from: self tagsBlock
]

{ #category : #tags }
GLMTBlockTags >> computeTagsFor: aGlamourValue from: aBlock [
	| value |
	
	aBlock ifNil: [ ^ OrderedCollection new ].
	value := aBlock glamourValue: aGlamourValue.
	^ value isString
		ifTrue: [ OrderedCollection with: value ]
		ifFalse: [ value asOrderedCollection ]
]

{ #category : #testing }
GLMTBlockTags >> hasTags [
	^ self tagsBlock notNil
]

{ #category : #testing }
GLMTBlockTags >> hasTagsFilter [
	^ self tagsFilterBlock notNil
]

{ #category : #scripting }
GLMTBlockTags >> tags: aBlock [

	self tagsBlock: aBlock.
	self hasTagsFilter ifFalse:[ self tagsFilter: aBlock ]
]

{ #category : #accessing }
GLMTBlockTags >> tagsBlock [
	self explicitRequirement
]

{ #category : #scripting }
GLMTBlockTags >> tagsBlock: anObject [
	self explicitRequirement
]

{ #category : #scripting }
GLMTBlockTags >> tagsFilter: anObject [
	self explicitRequirement
]

{ #category : #accessing }
GLMTBlockTags >> tagsFilterBlock [
	self explicitRequirement
]
