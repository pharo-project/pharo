"
Finds reference paths between objects to help find memory leaks. It finds the shortest paths between the objects. 

If you want to find a path between the ""Smalltalk"" global dictionary and some instance of a class, you can use the findPathToInstanceOf: method. For example, ""ReferenceFinder findPathToInstanceOf: WriteStream"" will find a reference path between Smalltalk and an instance of WriteStream.
"
Class {
	#name : #ReferenceFinder,
	#superclass : #Object,
	#instVars : [
		'backlinks',
		'objectsLeft',
		'testBlock',
		'foundBlock'
	],
	#category : #'ReferenceFinder-Core'
}

{ #category : #accessing }
ReferenceFinder class >> findAllPathsTo: anObject [ 
	"self findAllPathsTo: (Object compiledMethodAt: #printString)"

	^self findAllPathsTo: anObject from: Smalltalk
]

{ #category : #accessing }
ReferenceFinder class >> findAllPathsTo: anObject from: fromObject [ 
	"self findAllPathsTo: (Object compiledMethodAt: #printString) from: Object"

	^(self new)
		startFrom: fromObject;
		findAllPaths: [:each | each == anObject]
]

{ #category : #accessing }
ReferenceFinder class >> findAllPathsToInstanceOf: aBehavior [ 
	"self findAllPathsToInstanceOf: RefactoringBrowser"

	^self findAllPathsToInstanceOf: aBehavior from: Smalltalk
]

{ #category : #accessing }
ReferenceFinder class >> findAllPathsToInstanceOf: aBehavior from: fromObject [ 
	"self findAllPathsToInstanceOf: RefactoringBrowser from: ScheduledControllers"

	^(self new)
		startFrom: fromObject;
		findAllPaths: [:each | each class == aBehavior]
]

{ #category : #accessing }
ReferenceFinder class >> findPathTo: anObject [ 
	^self findPathTo: anObject from: Smalltalk
]

{ #category : #accessing }
ReferenceFinder class >> findPathTo: anObject from: fromObject [ 
	"self findPathTo: RefactoringManager instance from: Object"

	^(self new)
		startFrom: fromObject;
		findPath: [:each | each == anObject]
]

{ #category : #accessing }
ReferenceFinder class >> findPathToInstanceOf: aBehavior [ 
	^self findPathToInstanceOf: aBehavior from: Smalltalk
]

{ #category : #accessing }
ReferenceFinder class >> findPathToInstanceOf: aBehavior from: fromObject [ 
	"self findPathToInstanceOf: RefactoringManager from: Object"

	^(self new)
		startFrom: fromObject;
		findPath: [:each | each class == aBehavior]
]

{ #category : #private }
ReferenceFinder >> _object: anObject at: anInteger [
	"Answer the value of an indexable field in anObject. Fail if the
	 argument anInteger is not an Integer or is out of bounds."

	^anObject basicAt: anInteger
]

{ #category : #private }
ReferenceFinder >> _object: anObject instVarAt: anInteger [
	"Answer with a fixed variable in anObject.  The numbering of the variables
	 corresponds to the named instance variables.  Fail if the index is not an
	 Integer or is not the index of a fixed variable."

	^anObject instVarAt: anInteger
]

{ #category : #private }
ReferenceFinder >> _objectClass: anObject [
	"Answer the object which is the argument's class."

	^anObject class
]

{ #category : #private }
ReferenceFinder >> _objectSize: anObject [
	"Answer the number of indexable fields in the argument anObject.
	 This value is the same as the largest legal subscript."

	^anObject basicSize
]

{ #category : #private }
ReferenceFinder >> addBacklinkFrom: newObject to: anObject [ 
	backlinks last at: newObject put: anObject
]

{ #category : #private }
ReferenceFinder >> addBacklinkLevel [
	backlinks add: self identityDictionaryClass new
]

{ #category : #private }
ReferenceFinder >> backlinkFor: anObject [ 
	| toObject |
	backlinks do: 
			[:each | 
			toObject := each at: anObject ifAbsent: [nil].
			toObject notNil ifTrue: [^toObject]].
	^nil
]

{ #category : #private }
ReferenceFinder >> computePathFor: anObject [ 
	| path object |
	path := OrderedCollection new.
	object := anObject.
	[object isNil] whileFalse: 
			[path add: object.
			object := self backlinkFor: object].
	^path reverse
]

{ #category : #private }
ReferenceFinder >> find [
	self searchForObject
]

{ #category : #accessing }
ReferenceFinder >> findAllPaths: aBlock [ 
	| paths |
	paths := OrderedCollection new.
	foundBlock := [:path | paths add: path].
	testBlock := aBlock.
	self find.
	^paths
]

{ #category : #accessing }
ReferenceFinder >> findPath: aBlock [ 
	foundBlock := [:path | ^path].
	testBlock := aBlock.
	self find.
	^nil
]

{ #category : #private }
ReferenceFinder >> hasSearched: newObject [ 
	^backlinks anySatisfy: [:each | each includesKey: newObject]
]

{ #category : #private }
ReferenceFinder >> identityDictionaryClass [
	^IdentityDictionary
]

{ #category : #accessing }
ReferenceFinder >> ignore: anObject [ 
	self addBacklinkFrom: anObject to: anObject
]

{ #category : #'initialize-release' }
ReferenceFinder >> initialize [
	self startFrom: Smalltalk
]

{ #category : #private }
ReferenceFinder >> printToDoNotice [
	Transcript
		cr;
		show: 'Level ';
		show: backlinks size printString;
		show: ' with ';
		show: objectsLeft size printString;
		show: ' objects to search'
]

{ #category : #private }
ReferenceFinder >> processLinkTo: newObject from: anObject [ 
	newObject class instSize + newObject basicSize = 0 
		ifTrue: 
			[^(testBlock value: newObject) 
				ifTrue: 
					[(self computePathFor: anObject)
						add: newObject;
						yourself]
				ifFalse: [nil]].
	(self hasSearched: newObject) 
		ifFalse: 
			[(testBlock value: newObject) 
				ifTrue: 
					[^(self computePathFor: anObject)
						add: newObject;
						yourself].
			objectsLeft add: newObject.
			self addBacklinkFrom: newObject to: anObject].
	^nil
]

{ #category : #private }
ReferenceFinder >> searchForObject [
	
	[| objects |
	self printToDoNotice.
	objects := objectsLeft.
	objectsLeft := OrderedCollection new.
	self addBacklinkLevel.
	objects do: 
			[:each | 
			| path |
			(path := self searchVariablesIn: each) notNil 
				ifTrue: [foundBlock value: path].
			(path := self searchIndicesIn: each) notNil 
				ifTrue: [foundBlock value: path]].
	objectsLeft isEmpty] 
			whileFalse
]

{ #category : #private }
ReferenceFinder >> searchIndicesIn: anObject [
	| class |
	class := self _objectClass: anObject.
	class isBits ifTrue: [^nil].
	class isWeak ifTrue: [^nil].
	1 to: (self _objectSize: anObject) do:
		[:i | | path |
		path := self processLinkTo: (self _object: anObject at: i) from: anObject.
		path notNil ifTrue: [^path]].
	^nil
]

{ #category : #private }
ReferenceFinder >> searchVariablesIn: anObject [
	| class |
	class := self _objectClass: anObject.
	1 to: class instSize do:
		[:i | | path |
		path := self processLinkTo: (self _object: anObject instVarAt: i) from: anObject.
		path notNil ifTrue: [^path]].
	^nil
]

{ #category : #'initialize-release' }
ReferenceFinder >> startFrom: anObject [ 
	backlinks := OrderedCollection new.
	self addBacklinkLevel.
	objectsLeft := OrderedCollection with: anObject.
	backlinks last at: anObject put: nil
]
