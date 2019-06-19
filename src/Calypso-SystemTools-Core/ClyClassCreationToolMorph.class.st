"
I am a tool to create a class using textual class definition.

I retrieve a package and class tag from given context and use them in class definition.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classTag:		<Symbol>
	package:		<RPackage>
"
Class {
	#name : #ClyClassCreationToolMorph,
	#superclass : #ClyTextEditorToolMorph,
	#instVars : [
		'package',
		'classTag'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
}

{ #category : #testing }
ClyClassCreationToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isClassSelected not
]

{ #category : #accessing }
ClyClassCreationToolMorph class >> tabOrder [
	^1
]

{ #category : #operations }
ClyClassCreationToolMorph >> applyChanges [
	| newClass |
	newClass := browser compileANewClassFrom: self pendingText asString notifying: textMorph startingFrom: nil.
	newClass ifNil: [ ^false].
	
	self removeFromBrowser.
	browser selectClass: newClass.
	^true
]

{ #category : #testing }
ClyClassCreationToolMorph >> belongsToCurrentBrowserContext [

	| packageSelection |
	packageSelection := browser packageSelection.
	package ifNil: [ ^packageSelection isEmpty].
	packageSelection isEmpty ifTrue: [ ^true ].
	
	^packageSelection includesActualObject: package
]

{ #category : #accessing }
ClyClassCreationToolMorph >> classTag [
	^ classTag
]

{ #category : #accessing }
ClyClassCreationToolMorph >> classTag: anObject [
	classTag := anObject
]

{ #category : #building }
ClyClassCreationToolMorph >> classTemplate [

	| template |
	
	template := Slot showSlotClassDefinition
		ifTrue: [ 
			'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	package: ''' ]
		ifFalse: [ 
			'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' ].

	^ template , self packageName , '''' 
]

{ #category : #initialization }
ClyClassCreationToolMorph >> defaultIconName [
	^#add
]

{ #category : #initialization }
ClyClassCreationToolMorph >> defaultTitle [
	^'New class'
]

{ #category : #building }
ClyClassCreationToolMorph >> editingText [
	^self classTemplate 
]

{ #category : #building }
ClyClassCreationToolMorph >> fillStatusBar [ 
	super fillStatusBar.
	
	statusBar addCommandItem: (ClySlotClassDefinitionSwitchMorph for: self)
]

{ #category : #testing }
ClyClassCreationToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^package = anotherBrowserTool package 
		and: [ classTag = anotherBrowserTool classTag ]
]

{ #category : #accessing }
ClyClassCreationToolMorph >> package [
	^ package
]

{ #category : #accessing }
ClyClassCreationToolMorph >> package: anObject [
	package := anObject
]

{ #category : #accessing }
ClyClassCreationToolMorph >> packageName [

	package ifNil: [ ^'' ].

	classTag ifNil: [ ^package name ].

	^package name , '-' , classTag
]

{ #category : #printing }
ClyClassCreationToolMorph >> printContext [
	package ifNil: [^super printContext].	
	classTag ifNil: [ ^package name ].
	^package name, ' / ' , classTag
]

{ #category : #initialization }
ClyClassCreationToolMorph >> setUpModelFromContext [
	super setUpModelFromContext.

	context isPackageSelected ifTrue: [ package := context lastSelectedPackage ].
	context isClassTagSelected ifTrue: [ classTag := context lastSelectedClassTag]
]
