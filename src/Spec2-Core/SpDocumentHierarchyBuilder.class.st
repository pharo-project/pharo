Class {
	#name : #SpDocumentHierarchyBuilder,
	#superclass : #Object,
	#instVars : [
		'flattenTree',
		'builder',
		'filterBlock',
		'class'
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
		from: SpAbstractPresenter
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

{ #category : #testing }
SpDocumentHierarchyBuilder >> isPassingThrough: aClass topLevel: topClass [
	| superclasses |
	
	superclasses := flattenTree at: topClass superclass ifAbsent: [ #() ].
	superclasses size <= 1 ifFalse: [ 
		^ (superclasses indexOf: topClass) < superclasses size ].
	
	^ false
]

{ #category : #accessing }
SpDocumentHierarchyBuilder >> topClass [

	^ Model
]
