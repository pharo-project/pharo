"
I am an editor of class definition.
I allow to modify class properties (package, tags and variables) using text editor.

And in addition I allow to create new classes by typing new class name
"
Class {
	#name : 'ClyClassDefinitionEditorToolMorph',
	#superclass : 'ClyClassEditorToolMorph',
	#category : 'Calypso-SystemTools-Core-Editors-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Classes'
}

{ #category : 'operations' }
ClyClassDefinitionEditorToolMorph >> applyChanges [

	| code ast |
	code := self pendingText.

	"The next line are here for the compatibility with the old class definition."
	ast := OCParser parseFaultyExpression: code.
	(OldClassDefinitionBuilder isOldClassCreation: ast) ifTrue: [
		Warning signal:
			'The class definition used is the old Pharo class definition. Now the Fluid class definition should be used. Check the comment of ShiftClassBuilder to know the syntax.'.
		^ self applyChangesAsOldClassDefinition: ast ].

	^ (code asString lines first includesSubstring: '<<')
		  ifTrue: [ self applyChangesAsClassDefinition ]
		  ifFalse: [
			  self pendingText: code copy. "no idea why we have to copy"
			  self applyChangesAsMethodDefinition ]
]

{ #category : 'operations' }
ClyClassDefinitionEditorToolMorph >> applyChangesAsClassDefinition [

	| newClass |
	newClass := browser
		            compileANewClassFrom: self pendingText asString
		            notifying: textMorph
		            startingFrom: editingClass.

	newClass ifNil: [ ^ false ].
	browser selectClass: newClass.
	^ true
]

{ #category : 'operations' }
ClyClassDefinitionEditorToolMorph >> applyChangesAsMethodDefinition [

 	| newMethod selector selectedClass |
 	selectedClass := self editingClass.

 	selector := selectedClass
 		            compile: self pendingText asString
 		            notifying: textMorph.

 	selector ifNil: [ ^ false ].
 	newMethod := selectedClass >> selector.
 	MethodClassifier classify: newMethod.

 	self removeFromBrowser.
 	browser tabManager desiredSelection: { ClyMethodCodeEditorToolMorph }.
 	browser selectMethod: newMethod.
 	^ true
]

{ #category : 'operations' }
ClyClassDefinitionEditorToolMorph >> applyChangesAsOldClassDefinition: ast [

	| newClass |
	newClass := OldClassDefinitionBuilder buildFromAST: ast.

	newClass ifNil: [ ^ false ].
	browser selectClass: newClass.
	^ true
]

{ #category : 'to sort' }
ClyClassDefinitionEditorToolMorph >> createTextContext [
	^self selectedSourceNode
		ifNil: [ super createTextContext ]
		ifNotNil: [ :astNode | ClyClassDefinitionContext for: self selectedNode: astNode]
]

{ #category : 'building' }
ClyClassDefinitionEditorToolMorph >> decorateContainerTab [
	| title |
	super decorateContainerTab.
	title := editingClass name.

	editingClass isClassSide
		ifTrue: [ title := title asText allBold ].

	containerTab label: title
]

{ #category : 'initialization' }
ClyClassDefinitionEditorToolMorph >> defaultIconName [

	self editingClass isTrait ifTrue: [ ^ #trait ].
	^ #class
]

{ #category : 'to sort' }
ClyClassDefinitionEditorToolMorph >> editingText [
	"Answer a <String> with the receivers class definition"

	^ editingClass definitionString
]

{ #category : 'testing' }
ClyClassDefinitionEditorToolMorph >> isCommandAvailable: aCommand [

	^ aCommand canBeExecutedInClassEditor: self
]

{ #category : 'to sort' }
ClyClassDefinitionEditorToolMorph >> parseClassDefinition [

	^ CDFluidClassDefinitionParser parse: self editingText
]

{ #category : 'selecting text' }
ClyClassDefinitionEditorToolMorph >> selectSourceNode: aCDNode [
	textMorph setSelection: aCDNode sourceInterval
]

{ #category : 'selecting text' }
ClyClassDefinitionEditorToolMorph >> selectVariableNamed: aString [
	| classDefinition varNode |
	classDefinition := self parseClassDefinition.
	varNode := classDefinition children
		detect: [ :each | each isVariable and: [each name = aString]]
		ifNone: [ ^self ].
	self selectSourceNode: varNode
]

{ #category : 'accessing' }
ClyClassDefinitionEditorToolMorph >> selectedSourceNode [
	| selectedInterval definitionNode |
	selectedInterval := self selectedTextInterval.
	definitionNode := self parseClassDefinition.

	^(definitionNode bestNodeFor: selectedInterval)
		ifNil: [ definitionNode ]
]

{ #category : 'initialization' }
ClyClassDefinitionEditorToolMorph >> setUpModelFromContext [

	super setUpModelFromContext.
	editingClass := context selectedClassSide
]
