"
I'm reponsible to perform some basic core reparation on faulty AST.

Currenlty I'm only used by the OpalCompiler to fix UndeclaredVariable in interactive context.
I'm mainly legacy code, but still used by OpalCompiler, and was extracted from OCUndeclaredVariableWarning.

I'm quite limited and I need love to improve and be usable in other context (right-click on a error/warning for instance) and for other kind of reparations.

* node <ASTNode> the AST node to repair.
* requestor <???> the original requestor (to do reparation on)
"
Class {
	#name : 'OCCodeReparator',
	#superclass : 'Object',
	#instVars : [
		'node',
		'requestor'
	],
	#category : 'OpalCompiler-UI',
	#package : 'OpalCompiler-UI'
}

{ #category : 'correcting' }
OCCodeReparator >> declareClassVar [

	node methodNode methodClass instanceSide
		addClassVarNamed: node name asSymbol
]

{ #category : 'correcting' }
OCCodeReparator >> declareGlobal [
	Smalltalk globals at: node name asSymbol put: nil
]

{ #category : 'correcting' }
OCCodeReparator >> declareInstVar: name [
	"Declare an instance variable."
	node methodNode methodClass addInstVarNamed: name
]

{ #category : 'correcting' }
OCCodeReparator >> declareTempAndPaste: name [
	| insertion theTextString characterBeforeMark tempsMark newMethodNode |

	"Note: asking the requestor the source code curently compiled is rather ugly"
	theTextString := self requestor text.

	"We parse again the method displayed in the morph. The variable methodNode has the first version of the method, without temporary declarations. "
	newMethodNode := RBParser parseMethod: theTextString.

	"We check if there is a declaration of temporary variables"
	tempsMark :=  newMethodNode body  rightBar ifNil: [ node methodNode body start ].

	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].

	(theTextString at: tempsMark) = $| ifTrue:  [
		"Paste it before the second vertical bar"
		insertion := name, ' '.

		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
	] ifFalse: [
		"No bars - insert some with CR, tab"
		insertion := '| ' , name , ' |',String cr.
		characterBeforeMark = Character tab ifTrue: [ insertion := insertion , String tab ] ].

	self substituteWord: insertion wordInterval: (tempsMark to: tempsMark-1)
]

{ #category : 'correcting' }
OCCodeReparator >> defineClass: classSymbol [
	"Prompts the user to define a new class."

	| class classDefinition |
	class := node methodNode methodClass.
	classDefinition := ClassDefinitionPrinter fluid classDefinitionTemplateInPackage: class package name tag: class packageTag name named: classSymbol.
	^ self defineClassOrTrait: classSymbol definitionString: classDefinition
]

{ #category : 'correcting' }
OCCodeReparator >> defineClassOrTrait: aSymbol definitionString: aString [
	"Prompts the user to define a new class oe trait."

	| classBinding result definitionString |
	definitionString := MorphicUIManager new
		multiLineRequest: 'Edit definition:'
		initialAnswer: aString
		answerHeight: 200.
	definitionString isEmptyOrNil
		ifTrue: [ ^ Abort signal ].
	result := self class compiler
		source: definitionString;
		logged: true;
		evaluate.
	"Because some class definition syntax the (fuild one) does not evaluate to a
	class but a class builder, we must call `fluidInstall`.
	(that is a noop on real classes)."
	result fluidInstall.
	classBinding := node owningScope lookupVar: aSymbol.
	"make sure to recompile all methods referencing this class"
	classBinding usingMethods do: [:method | method recompile].
	^classBinding
]

{ #category : 'correcting' }
OCCodeReparator >> defineTrait: traitName [
	"Prompts the user to define a new trait."

	| traitSymbol class traitDefinition |
	traitSymbol := traitName asSymbol.

	class := node methodNode methodClass.
	traitDefinition := ClassDefinitionPrinter fluid traitDefinitionTemplateInPackage: class package name tag: class packageTag name named: traitSymbol.
	^ self defineClassOrTrait: traitSymbol definitionString: traitDefinition
]

{ #category : 'accessing' }
OCCodeReparator >> node [

	^ node
]

{ #category : 'accessing' }
OCCodeReparator >> node: anObject [

	node := anObject
]

{ #category : 'menu morph' }
OCCodeReparator >> openMenu [
	"Display a menu to perform various possible reparations on undefined variables.
	* Return true if a reparation was done and a recompilation is needed.
	* Return nil if the undefined variable should stays undefined.
	* Return fail if the user cancel"

	| alternatives labels actions lines caption choice name interval |

	interval := node sourceInterval.
	name := node name.
	alternatives := self possibleVariablesFor: name.
	labels := OrderedCollection new.
	actions := OrderedCollection new.
	lines := OrderedCollection new.
	name first isLowercase
		ifTrue: [
			labels add: 'Declare new temporary variable'.
			actions add: [ self declareTempAndPaste: name ].
			labels add: 'Declare new instance variable'.
			actions add: [ self declareInstVar: name ] ]
		ifFalse: [
			labels add: 'Leave variable undeclared'.
			actions add: [ ^ nil ].
			lines add: labels size.
			labels add: 'Define new class'.
			actions
				add: [
					[ self defineClass: name asSymbol ]
						on: Abort
						do: [ self openMenu ] ].
			labels add: 'Declare new global'.
			actions add: [ self declareGlobal ].
			requestor isScripting ifFalse:
				[labels add: 'Declare new class variable'.
				actions add: [ self declareClassVar ]].
			labels add: 'Define new trait'.
			actions
				add: [
					[ self defineTrait: name asSymbol ]
						on: Abort
						do: [ self openMenu ] ] ].
	lines add: labels size.
	alternatives
		do: [ :each |
			labels add: each.
			actions
				add: [
					self substituteVariable: each atInterval: interval ] ].
	lines add: labels size.
	labels add: 'Cancel'.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := MorphicUIManager new chooseFrom: labels lines: lines title: caption.
	(actions at: choice ifAbsent: [ ^ false ]) value.
	^ true
]

{ #category : 'correcting' }
OCCodeReparator >> possibleVariablesFor: proposedVariable [
	| results class |
	class := node methodNode methodClass.

	results := proposedVariable correctAgainst: node methodOrBlockNode scope allTempNames
								continuedFrom: nil.
	proposedVariable isValidGlobalName ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results
]

{ #category : 'accessing' }
OCCodeReparator >> requestor [

	^ requestor
]

{ #category : 'accessing' }
OCCodeReparator >> requestor: anObject [

	requestor := anObject
]

{ #category : 'correcting' }
OCCodeReparator >> substituteVariable: varName atInterval: anInterval [
	self
		substituteWord: varName
		wordInterval: anInterval.
	node methodNode source: self requestor text.
	node replaceWith:((RBVariableNode named: varName) binding: (node owningScope lookupVar: varName))
]

{ #category : 'correcting' }
OCCodeReparator >> substituteWord: correctWord wordInterval: spot [
	"Substitute the correctSelector into the (presuamed interactive) receiver."

	self requestor correctFrom: (spot first)
					to: (spot last)
					with: correctWord
]
