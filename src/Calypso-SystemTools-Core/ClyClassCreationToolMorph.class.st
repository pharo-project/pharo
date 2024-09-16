"
I am a tool to create a class using textual class definition.

I retrieve a package and class tag from given context and use them in class definition.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classTag:		<Symbol>
	package:		<Package>
"
Class {
	#name : 'ClyClassCreationToolMorph',
	#superclass : 'ClyTextEditorToolMorph',
	#instVars : [
		'package',
		'tag'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Classes'
}

{ #category : 'testing' }
ClyClassCreationToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isClassSelected not
]

{ #category : 'accessing' }
ClyClassCreationToolMorph class >> tabOrder [
	^1
]

{ #category : 'operations' }
ClyClassCreationToolMorph >> applyChanges [

	| newClass |
	newClass := browser
						compileANewClassFrom: self pendingText asString
						notifying: textMorph
						startingFrom: nil.
	newClass ifNil: [ ^false].

	self removeFromBrowser.
	browser selectClass: newClass.
	^true
]

{ #category : 'testing' }
ClyClassCreationToolMorph >> belongsToCurrentBrowserContext [

	| packageSelection |
	packageSelection := browser packageSelection.
	package ifNil: [ ^packageSelection isEmpty].
	packageSelection isEmpty ifTrue: [ ^true ].

	^packageSelection includesActualObject: package
]

{ #category : 'template' }
ClyClassCreationToolMorph >> classTemplate [

	^ ClassDefinitionPrinter fluid compactClassDefinitionTemplateInPackage:self packageName tag: self tag
]

{ #category : 'initialization' }
ClyClassCreationToolMorph >> defaultIconName [
	^#add
]

{ #category : 'initialization' }
ClyClassCreationToolMorph >> defaultTitle [
	^'New class'
]

{ #category : 'building' }
ClyClassCreationToolMorph >> editingText [
	^self classTemplate
]

{ #category : 'testing' }
ClyClassCreationToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].

	^package = anotherBrowserTool package
		and: [ tag = anotherBrowserTool tag ]
]

{ #category : 'testing' }
ClyClassCreationToolMorph >> isValidInContext: aClyFullBrowserContext [
	| isValid |

	self context class = aClyFullBrowserContext class
		ifFalse: [ ^ false ].

	isValid := true.
	aClyFullBrowserContext isPackageSelected
		ifTrue: [ isValid := package = aClyFullBrowserContext lastSelectedPackage ].

	^ isValid and: [ aClyFullBrowserContext isPackageTagSelected
		ifTrue: [ tag = aClyFullBrowserContext lastSelectedPackageTag ]
		ifFalse: [ tag isNil ] ]
]

{ #category : 'accessing' }
ClyClassCreationToolMorph >> package [
	^ package
]

{ #category : 'accessing' }
ClyClassCreationToolMorph >> package: anObject [
	package := anObject
]

{ #category : 'accessing' }
ClyClassCreationToolMorph >> packageName [

	^ package
		  ifNotNil: [ package name ]
		  ifNil: [ '' ]
]

{ #category : 'printing' }
ClyClassCreationToolMorph >> printContext [
	package ifNil: [^super printContext].
	tag ifNil: [ ^package name ].
	^package name, ' / ' , tag
]

{ #category : 'initialization' }
ClyClassCreationToolMorph >> resetStateForRemoval [
	" A package was removed from the receiver's browser, update the receiver's text "

	self package: nil.
	textModel setInitialText: self classTemplate.
	textMorph hasEditingConflicts: false.
	textMorph changed.

]

{ #category : 'initialization' }
ClyClassCreationToolMorph >> setUpModelFromContext [
	super setUpModelFromContext.

	context isPackageSelected ifTrue: [ package := context lastSelectedPackage ].
	context isPackageTagSelected ifTrue: [ tag := context lastSelectedPackageTag]
]

{ #category : 'accessing' }
ClyClassCreationToolMorph >> tag [
	^ tag
]

{ #category : 'accessing' }
ClyClassCreationToolMorph >> tag: anObject [
	tag := anObject
]
