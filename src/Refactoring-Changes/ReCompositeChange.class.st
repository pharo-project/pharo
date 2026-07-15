"
I am a composition of RBRefactoryChanges. I hold a list of changes. 
I implement many ""environment change"" operation like renameClass/removeClass/ rename instance variable that are
actually called on the name space by a refactoring operation and my responsibilty is to create a appropriate refactoring change
and save it in my list of changes.

"
Class {
	#name : 'ReCompositeChange',
	#superclass : 'ReChange',
	#instVars : [
		'changes',
		'newName',
		'oldName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReCompositeChange class >> named: aString [

	^ self new
		  name: aString;
		  yourself
]

{ #category : 'comparing' }
ReCompositeChange >> = aRefactoryBuilder [
	self class = aRefactoryBuilder class
		ifFalse: [ ^ false ].
	changes size = aRefactoryBuilder changes size
		ifFalse: [ ^ false ].
	changes with: aRefactoryBuilder changes do: [ :first :second |
		first = second
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'accessing' }
ReCompositeChange >> addChange: aRefactoryChange [

	changes add: aRefactoryChange.
	^ aRefactoryChange
]

{ #category : 'refactory - changes' }
ReCompositeChange >> addClassVariable: variableName to: aClass [

	^ self addChange:
		  (changeFactory addClassVariable: variableName to: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> addInstanceVariable: variableName to: aClass [

	^ self addChange:
		  (changeFactory addInstanceVariable: variableName to: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> addPackageNamed: aString [

	^ self addChange: (changeFactory addPackageNamed: aString)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> addPool: aPoolVariable to: aClass [

	^ self addChange:
		  (changeFactory addPoolVariable: aPoolVariable to: aClass)
]

{ #category : 'accessing' }
ReCompositeChange >> addProtocolNamed: aString in: aClass [

	^ self addChange:
		  (ReAddProtocolChange addProtocolNamed: aString in: aClass)
]

{ #category : 'accessing' }
ReCompositeChange >> changeClass [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReCompositeChange >> changeForClass: aClassName selector: aSelector [

	changes reverseDo: [ :each | (each changeForClass: aClassName selector: aSelector) ifNotNil: [ :change | ^ change ] ].
	^ nil
]

{ #category : 'accessing' }
ReCompositeChange >> changeForMetaclass: aClassName selector: aSelector [

	changes reverseDo: [ :each | (each changeForMetaclass: aClassName selector: aSelector) ifNotNil: [ :change | ^ change ] ].
	^ nil
]

{ #category : 'private - inspector accessing' }
ReCompositeChange >> changes [

	^ changes
]

{ #category : 'private - inspector accessing' }
ReCompositeChange >> changes: aCollection [

	changes := aCollection
]

{ #category : 'accessing' }
ReCompositeChange >> changesSize [

	^ changes inject: 0 into: [ :sum :each | sum + each changesSize ]
]

{ #category : 'refactory - changes' }
ReCompositeChange >> changesToDisplayIn: aBrowser [

	^ self changes flatCollect: [:each | each changesToDisplayIn: aBrowser ]
]

{ #category : 'refactory - changes' }
ReCompositeChange >> comment: aString in: aClass [

	^ self addChange: (changeFactory comment: aString in: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> compile: source in: class [

	^ self addChange: (changeFactory addMethodSource: source in: class)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> compile: source in: class classified: aProtocol [

	^ self addChange:
		  (changeFactory
			   addMethodSource: source
			   in: class
			   classified: aProtocol)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> defineClass: definition [

	^ self addChange: (changeFactory addClassDefinition: definition)
]

{ #category : 'private' }
ReCompositeChange >> executeNotifying: aBlock [

	| undos undo |
	undos := changes collect: [ :each | each executeNotifying: aBlock ].
	undo := self copy.
	undo changes: undos reversed.
	^ undo
]

{ #category : 'comparing' }
ReCompositeChange >> hash [

	^ self class hash bitXor: self changes size hash
]

{ #category : 'initialization' }
ReCompositeChange >> initialize [

	super initialize.
	changes := OrderedCollection new
]

{ #category : 'testing' }
ReCompositeChange >> isInScope: aRBBrowserEnvironment [ 
	^ aRBBrowserEnvironment includesClass: self changeClass.
]

{ #category : 'accessing' }
ReCompositeChange >> newName [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReCompositeChange >> oldName [

	^ self subclassResponsibility
]

{ #category : 'copying' }
ReCompositeChange >> postCopy [

	super postCopy.
	changes := changes collect: [ :each | each copy ]
]

{ #category : 'printing' }
ReCompositeChange >> printOn: aStream [

	super printOn: aStream.
	name ifNotNil: [ aStream nextPutAll: name ]
]

{ #category : 'accessing' }
ReCompositeChange >> problemCount [

	^ self changesSize
]

{ #category : 'accessing' }
ReCompositeChange >> removeChange: aChange [

	^ changes remove: aChange ifAbsent: [ ]
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removeClass: aClass [

	^ self addChange: (changeFactory removeClassNamed: aClass name)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removeClassNamed: aSymbol [

	^ self addChange: (changeFactory removeClassNamed: aSymbol)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removeClassVariable: variableName from: aClass [

	^ self addChange:
		  (changeFactory removeClassVariable: variableName from: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removeInstanceVariable: variableName from: aClass [

	^ self addChange:
		  (changeFactory removeInstanceVariable: variableName from: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removeMethod: aSelector from: aClass [

	^ self addChange:
		  (changeFactory removeMethod: aSelector from: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> removePool: aPoolVariable from: aClass [

	^ self addChange:
		  (changeFactory removePoolVariable: aPoolVariable from: aClass)
]

{ #category : 'accessing' }
ReCompositeChange >> removeProtocolNamed: aString in: aClass [
	self flag: #todo.
	"This change is not made via the changefactory!!!"
	^ self addChange: (ReRemoveProtocolChange removeProtocolNamed: aString in: aClass)
]

{ #category : 'accessing' }
ReCompositeChange >> renameChangesForClass: oldClassName to: newClassName [

	^ self copy
		  changes: (self changes collect: [ :each |
					   each renameChangesForClass: oldClassName to: newClassName ]);
		  yourself
]

{ #category : 'refactory - changes' }
ReCompositeChange >> renameClass: class to: newNameLocal [

	^ self addChange: (changeFactory renameClass: class to: newNameLocal)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> renameClassVariable: oldNameLocal to: newNameLocal in: aClass [

	^ self addChange: (changeFactory
			   renameClassVariable: oldNameLocal
			   to: newNameLocal
			   in: aClass)
]

{ #category : 'refactory - changes' }
ReCompositeChange >> renameInstanceVariable: oldNameLocal to: newNameLocal in: aClass [

	^ self addChange: (changeFactory
			   renameInstanceVariable: oldNameLocal
			   to: newNameLocal
			   in: aClass)
]

{ #category : 'accessing' }
ReCompositeChange >> selector: aSelector in: aClass classified: aProtocol [
	self flag: #todo.
	"This change is not made via the changefactory!!!"
	^ self addChange: (ReMethodProtocolChange
							selector: aSelector
							in: aClass
							classified: aProtocol)
]
