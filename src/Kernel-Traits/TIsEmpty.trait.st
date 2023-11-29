"
I am a trait used for object responding to #isEmpty to bring the rest of the behavior we might expect such as #isNotEmpty, #ifEmpty: #ifNotEmpty:...
"
Trait {
	#name : 'TIsEmpty',
	#category : 'Kernel-Traits-Base',
	#package : 'Kernel-Traits',
	#tag : 'Base'
}

{ #category : 'testing' }
TIsEmpty >> ifEmpty: aBlock [

	^ self isEmpty
		  ifTrue: [ aBlock value ]
		  ifFalse: [ self ]
]

{ #category : 'testing' }
TIsEmpty >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise.
	If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty
		  ifTrue: [ emptyBlock value ]
		  ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : 'testing' }
TIsEmpty >> ifNotEmpty: aBlock [

	^ self isEmpty
		  ifTrue: [ self ]
		  ifFalse: [ aBlock cull: self ]
]

{ #category : 'testing' }
TIsEmpty >> ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty
		  ifTrue: [ emptyBlock value ]
		  ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : 'testing' }
TIsEmpty >> isEmpty [
	"Evaluate the given block, answering its value if the receiver is empty, otherwise answer the receiver.
	Note that the fact that this method returns its receiver in case the receiver is not empty allows one to write expressions like the following ones: self classifyMethodAs: (myProtocol ifEmpty: [ Protocol unclassified ])"

	^ self explicitRequirement
]

{ #category : 'testing' }
TIsEmpty >> isNotEmpty [
	"Evaluate the given block with the receiver as argument, answering its value
	unless the receiver is empty, in which case answer the receiver."

	^ self isEmpty not
]
