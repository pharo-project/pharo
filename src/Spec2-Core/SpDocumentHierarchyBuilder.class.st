"
Create a hierarchy tree to show in microdown documentation.
This builder allows you to create the nice hierarchy you can see at `SpAbstractWidgetPresenter` class comment (in hierarchy section).

## Example
```Smalltalk
SpDocumentHierarchyBuilder new
	""The class where to start the hierarchy (a superclass of aClass)""
	fromClass: aTopClass;
	""The microdown builder"" 
	builder: aBuilder;	
	""A filter to make sure we include classes we want in hierarchy"" 
	filter: [ :eachClass | eachClass package packageName beginsWith: 'Spec2-' ]; 
	""Build the hierarchy for the class aClass""
	buildFor: aClass
```
"
Class {
	#name : #SpDocumentHierarchyBuilder,
	#superclass : #Object,
	#instVars : [
		'topClass',
		'flattenTree',
		'builder',
		'filterBlock',
		'class',
		'fromClass'
	],
	#category : #'Spec2-Core-Utils'
}

{ #category : #private }
SpDocumentHierarchyBuilder >> addLevel: level from: aClass [
	"'├ ─ ╰ │'"
	| path |

	path := (aClass allSuperclasses copyUpTo: self topClass) reversed.
	
	builder monospace: '  '.
	
	path do: [ :each |
		builder monospace: ((self isPassingThrough: aClass topLevel: each)
			ifTrue: [  '│ ' ]
			ifFalse: [ '  ' ]) ].
		
	level > 0 ifTrue: [ 
		| list |
		list := flattenTree at: aClass superclass ifAbsent: [ #() ].
		list ifNotEmpty: [ 
			builder monospace: ((list size = 1 or: [ list last = aClass ])
				ifTrue: [ '╰─ ' ]
				ifFalse: [ '├─ ' ]) ] ].

	builder monospace: aClass name.
	aClass = class 
		ifTrue: [ builder monospace: ' (this is me)' ].
	builder newLine.
	(flattenTree at: aClass) do: [ :each |
		self 
			addLevel: level + 1 
			from: each ]
]

{ #category : #private }
SpDocumentHierarchyBuilder >> addLevel: level from: aClass to: stream [
	"'├ ─ ╰ │'"
	| path |

	path := (aClass allSuperclasses copyUpTo: self topClass) reversed.
	
	path do: [ :each |
		stream << ((self isPassingThrough: aClass topLevel: each)
			ifTrue: [  '│ ' ]
			ifFalse: [ '  ' ]) ].
		
	level > 0 ifTrue: [ 
		| list |
		list := flattenTree at: aClass superclass ifAbsent: [ #() ].
		list ifNotEmpty: [ 
			(list size = 1 or: [ list last = aClass ])
				ifTrue: [ stream << '╰─ ' ]
				ifFalse: [ stream << '├─ ' ] ] ].

	stream << aClass name.
	stream newLine.
	(flattenTree at: aClass) do: [ :each |
		self 
			addLevel: level + 1 
			from: each 
			to: stream ]
]

{ #category : #private }
SpDocumentHierarchyBuilder >> applyFilterTo: aCollection [

	filterBlock ifNil: [ ^ aCollection ].
	^ aCollection select: filterBlock
]

{ #category : #building }
SpDocumentHierarchyBuilder >> buildFor: aClass [

	self fillTreeOf: aClass.
	self 
		addLevel: 0
		from: self fromClass
]

{ #category : #building }
SpDocumentHierarchyBuilder >> buildStringFor: aClass [
	
	self fillTreeOf: aClass.
	^ String streamContents: [ :stream |
		self 
			addLevel: 0
			from: SpAbstractPresenter 
			to: (ZnNewLineWriterStream on: stream) ]
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> builder: aBuilder [

	builder := aBuilder
]

{ #category : #private }
SpDocumentHierarchyBuilder >> fillTreeOf: aClass [

	class := aClass.
	flattenTree := OrderedDictionary new.
	self fillTreeWithSuperclassesOf: aClass.
	self fillTreeWithSubclassesOf: aClass.
	
	^ flattenTree
]

{ #category : #private }
SpDocumentHierarchyBuilder >> fillTreeWithSubclassesOf: aClass [
		
	flattenTree at: aClass put: (self applyFilterTo: aClass subclasses).
	aClass subclasses do: [ :each |
		self fillTreeWithSubclassesOf: each ]
]

{ #category : #private }
SpDocumentHierarchyBuilder >> fillTreeWithSuperclassesOf: aClass [
	| superclasses |

	superclasses := (aClass allSuperclasses copyUpTo: self topClass) reversed.
	superclasses do: [ :each | 
		flattenTree 
			at: each 
			put: { (superclasses 
				after: each
				ifAbsent: [ aClass ]) } ].
	
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> filter: aBlock [

	filterBlock := aBlock
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> fromClass [

	 ^  fromClass ifNil: [ SpAbstractPresenter ]
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> fromClass: aClass [

	fromClass := aClass
]

{ #category : #testing }
SpDocumentHierarchyBuilder >> isPassingThrough: aClass topLevel: aTopClass [
	| superclasses |
	
	superclasses := flattenTree at: aTopClass superclass ifAbsent: [ #() ].
	superclasses size <= 1 ifFalse: [ 
		^ (superclasses indexOf: aTopClass) < superclasses size ].
	
	^ false
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> topClass [

	^ topClass ifNil: [ self fromClass superclass ]
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> topClass: aClass [

	topClass := aClass
]
