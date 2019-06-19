"
I am a tool to create methods using source code editor.

I am opened in browser when class is selected. But in addition I disappear when there is selected method (it can be changed in the future).

Internal Representation and Key Implementation Points.

    Instance Variables
	methodClass:		<CompiledMethod>

"
Class {
	#name : #ClyMethodCreationToolMorph,
	#superclass : #ClyMethodEditorToolMorph,
	#instVars : [
		'methodClass'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #testing }
ClyMethodCreationToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isClassSelected
]

{ #category : #accessing }
ClyMethodCreationToolMorph class >> tabOrder [
	^ 1000
]

{ #category : #accessing }
ClyMethodCreationToolMorph >> activationPriority [
	"We want creation tool be active by default only when user select method group.
	Instead when user select class creation tool should not be activated"
	^browser methodGroupSelection isEmpty 
		ifTrue: [ 0]
		ifFalse: [ ClyClassDefinitionEditorToolMorph tabOrder + 1 ]
]

{ #category : #operations }
ClyMethodCreationToolMorph >> applyChanges [
	| newMethod selector selectedClass |
	selectedClass := self chooseClassForNewMethodIfNone: [^false].
	
	selector := methodTags 
		ifEmpty: [ selectedClass compile: self pendingText asString notifying: textMorph]
		ifNotEmpty: [
			selectedClass compile: self pendingText asString classified: methodTags anyOne notifying: textMorph].
			
	selector ifNil: [ ^false ].
	
	newMethod := selectedClass >> selector.
	methodTags ifEmpty: [ 
		MethodClassifier classify: newMethod.
		methodTags := newMethod tags ].
	self tagAndPackageEditingMethod: newMethod.
	
	self removeFromBrowser.
	browser tabManager desiredSelection: {ClyMethodCodeEditorToolMorph}.
	browser selectMethod: newMethod.
	^true
]

{ #category : #testing }
ClyMethodCreationToolMorph >> belongsToCurrentBrowserContext [
	^browser isClassSelected: methodClass instanceSide
]

{ #category : #initialization }
ClyMethodCreationToolMorph >> decorateContainerTab [
	| title |
	super decorateContainerTab.
	
	browser metaLevelScope == ClyClassSideScope ifFalse: [ ^self].
	 
	title := self defaultTitle asText allBold asStringMorph. 
	containerTab label: title
]

{ #category : #initialization }
ClyMethodCreationToolMorph >> defaultIconName [
	^#add
]

{ #category : #initialization }
ClyMethodCreationToolMorph >> defaultTitle [

	^browser metaLevelScope defaultName capitalized, ' method'
]

{ #category : #accessing }
ClyMethodCreationToolMorph >> editingText [
	^methodClass sourceCodeTemplate
]

{ #category : #testing }
ClyMethodCreationToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^methodClass == anotherBrowserTool methodClass
		and: [ methodTags = anotherBrowserTool methodTags 
				and: [ extendingPackage = anotherBrowserTool extendingPackage ] ]
]

{ #category : #accessing }
ClyMethodCreationToolMorph >> methodClass [
	^methodClass
]

{ #category : #testing }
ClyMethodCreationToolMorph >> modifiesExtension [
	"I create new method. So I am not modify anything"
	^false
]

{ #category : #printing }
ClyMethodCreationToolMorph >> printContext [
	^methodClass printSystemPath, ' / ' , methodClass name
]

{ #category : #controlling }
ClyMethodCreationToolMorph >> restoreBrowserState [

	browser selectClass: methodClass
]

{ #category : #initialization }
ClyMethodCreationToolMorph >> setUpModelFromContext [
	| selectedGroup |
	super setUpModelFromContext.

	methodClass := context selectedClassSide.
	context isMethodGroupSelected ifFalse: [ ^self].
	selectedGroup := context lastSelectedMethodGroup.
	(selectedGroup isKindOf: ClyExternalPackageMethodGroup) ifTrue: [ 
		^extendingPackage := selectedGroup package].
	(selectedGroup isKindOf: ClyTaggedMethodGroup) ifTrue: [ 
		^methodTags := {selectedGroup tag}]
]

{ #category : #initialization }
ClyMethodCreationToolMorph >> setUpTargetClasses [

	targetClasses := browser chooseClassesForNewMethod.
	(targetClasses includes: methodClass) ifFalse: [ 
		targetClasses := {methodClass}, targetClasses ]
]
