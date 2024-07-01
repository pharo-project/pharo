"
I'm a nice class hierarchy printer.
I can be configured as follows: 
	- show or not class instance variables (`ClassHierarchy>>#doNotShowState`)
	- show or not superclasses (`ClassHierarchy>>#doNotShowState`)
	- limit the classes to a given set
	- exclude some classes
	
For examples browse `ClassHierarchyPrinterTest`

```
ClassHierarchyPrinter new 
		forClass: ASTNode; 
		doNotShowState;
		doNotShowSuperclasses;
		excludedClasses: (ASTNode withAllSubclasses select: [ :each | each name beginsWith: 'RBPattern' ]);
		limitedToClasses: (ASTNode withAllSubclasses select: [:each | (each name beginsWith: 'AST') or: [ each name beginsWith: 'RB' ]]]).
```

produces 

```
'ASTNode
	RBProgramNode
		RBComment
		RBMethodNode
		RBPragmaNode
		RBReturnNode
		RBSequenceNode
		RBValueNode
			RBArrayNode
			RBAssignmentNode
			RBBlockNode
			RBCascadeNode
			RBLiteralNode
				RBLiteralArrayNode
				RBLiteralValueNode
			RBMessageNode
			RBParseErrorNode
				RBEnglobingErrorNode
					RBArrayErrorNode
					RBBlockErrorNode
					RBInvalidCascadeErrorNode
					RBLiteralArrayErrorNode
					RBLiteralByteArrayErrorNode
					RBMissingOpenerErrorNode
					RBParenthesesErrorNode
					RBPragmaErrorNode
					RBTemporariesErrorNode
					RBUnfinishedStatementErrorNode
					RBUnreachableStatementErrorNode
			RBSelectorNode
			RBVariableNode
				RBArgumentNode
				RBGlobalNode
				RBInstanceVariableNode
				RBSelfNode
				RBSuperNode
				RBTemporaryNode
				RBThisContextNode
	RBSlotInitializationNode
'
```	
### Some nice examples
	
Another nice example is the following one which prints a package roots by roots.

```
String streamContents: [ :st |  
	| printer |
	printer := ClassHierarchyPrinter new.
	printer doNotShowSuperclasses.
	printer on: st.	
	((SortedCollection sortBlock: [ :a :b | a name < b name ])
		addAll: (PackageOrganizer default packageNamed: 'Refactoring-Core') roots)
			do: [ :each | 
				printer forClass: each. 
				printer print.
				printer resetLevel]]
```

Now to print according to tags:

```
| p roots |
p := (PackageOrganizer default packageNamed: 'MenuRegistration').
roots := p roots. 
String streamContents: [ :st |  
	| printer |
	printer := ClassHierarchyPrinter new.
	printer doNotShowSuperclasses.
	printer on: st.    
	p classTags do: [ :aTag | 
		| rootsPerTag |
		printer 
			nextPutAll: 'Tag: ', aTag name ; 
			cr;
			nextPutAll: '==================================='; cr.
		  rootsPerTag := (p classesForClassTag: aTag name) select: [ :each | roots includes: each ].
        rootsPerTag
             do: [ :each | 
                    printer forClass: each. 
                    printer print.
                    printer resetLevel]]]
```


"
Class {
	#name : 'ClassHierarchyPrinter',
	#superclass : 'Object',
	#instVars : [
		'theClass',
		'excludedClasses',
		'limitedToClasses',
		'stream',
		'index',
		'showSuperclasses',
		'showState'
	],
	#category : 'Kernel-ExtraUtils-ForPharo',
	#package : 'Kernel-ExtraUtils',
	#tag : 'ForPharo'
}

{ #category : 'instance creation' }
ClassHierarchyPrinter class >> forClass: aClass [

	^ self new forClass: aClass; yourself
]

{ #category : 'public API' }
ClassHierarchyPrinter >> cr [
	stream cr
]

{ #category : 'configuration' }
ClassHierarchyPrinter >> doNotShowState [

	showState := false
]

{ #category : 'configuration' }
ClassHierarchyPrinter >> doNotShowSuperclasses [

	showSuperclasses := false
]

{ #category : 'configuration' }
ClassHierarchyPrinter >> excludedClasses: aCollection [
	"To set the classes that we do not want to see printed."
	excludedClasses := aCollection
]

{ #category : 'public API' }
ClassHierarchyPrinter >> forClass: aClass [
	theClass := aClass
]

{ #category : 'initialization' }
ClassHierarchyPrinter >> initialize [
	super initialize.
	excludedClasses := #().
	limitedToClasses := #().
	self resetLevel.
	showSuperclasses := true.
	showState := true.
	self initializeStream
]

{ #category : 'initialization' }
ClassHierarchyPrinter >> initializeStream [
	stream := (String new: 100) writeStream
]

{ #category : 'configuration' }
ClassHierarchyPrinter >> limitedToClasses: aCollection [
	"To set the only classes that should be printed."

	limitedToClasses := aCollection
]

{ #category : 'public API' }
ClassHierarchyPrinter >> nextPutAll: aString [
	stream nextPutAll: aString
]

{ #category : 'public API' }
ClassHierarchyPrinter >> on: aStream [

	stream := aStream
]

{ #category : 'public API' }
ClassHierarchyPrinter >> print [
	"Answer a description containing the names and instance variable names
	of all of the subclasses and superclasses of the class."

	theClass ifNotNil: [
		showSuperclasses
			ifTrue: [ self printSuperclasses ].
			self printSubclassesOf: theClass ].
	^ stream contents
]

{ #category : 'printing' }
ClassHierarchyPrinter >> printClass: aClass [
	"Print a single line in the printout."
	stream tab: index.
	stream nextPutAll: aClass name.
	aClass instVarNames ifNotEmpty: [ :vars |
		showState ifTrue: [ stream space.
								stream nextPutAll: '( '.
								aClass instVarNames
									do: [ :iv | stream nextPutAll:  iv asString ]
									separatedBy: [ stream space ].
								stream  nextPutAll: ' )' ] ].
	stream cr
]

{ #category : 'printing' }
ClassHierarchyPrinter >> printSubclassesOf: aClass [
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times."

	| sortedSubclasses |
	limitedToClasses ifNotEmpty: [
			(limitedToClasses includes: aClass)
				ifFalse: [ ^ self ]
			].
	(excludedClasses includes: aClass)
		ifTrue: [ ^ self ].

	self printClass: aClass.
	aClass == Class
		ifTrue: [ stream
				tab: index + 1;
				nextPutAll: '[ ... all the Metaclasses ... ]'.
			^ self ].
	sortedSubclasses := aClass subclasses
		asSortedCollection: [ :c1 :c2 | c1 name <= c2 name ].
	sortedSubclasses do: [ :subclass |
			index := index + 1.
			self printSubclassesOf: subclass.
			index := index - 1 ]
]

{ #category : 'printing' }
ClassHierarchyPrinter >> printSuperclasses [

	theClass allSuperclasses
		reverseDo: [ :aClass |
			self printClass: aClass.
			index := index + 1 ].
	stream cr.	"to spot the requested class."
]

{ #category : 'initialization' }
ClassHierarchyPrinter >> resetLevel [
	index := 0
]

{ #category : 'configuration' }
ClassHierarchyPrinter >> showSuperclasses [

	showSuperclasses := true
]

{ #category : 'public API' }
ClassHierarchyPrinter >> tab [
	stream tab
]
