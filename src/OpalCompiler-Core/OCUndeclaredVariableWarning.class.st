"
I get signalled when a temporary variable is used that is not defined.  My default action is to create an Undeclared binding and add it to the Undeclared dictionary.
"
Class {
	#name : #OCUndeclaredVariableWarning,
	#superclass : #OCSemanticWarning,
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #correcting }
OCUndeclaredVariableWarning >> declareClassVar [

	self methodClass instanceSide
		addClassVarNamed: node name asSymbol.
	(ReparseAfterSourceEditing new newSource: self requestor text) signal
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> declareGlobal [
	Smalltalk globals at: node name asSymbol put: nil.
	(ReparseAfterSourceEditing new newSource: self requestor text) signal.
	^Smalltalk globals bindingOf: node name asSymbol
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> declareInstVar: name [
	"Declare an instance variable."
	self methodClass addInstVarNamed: name.
	(ReparseAfterSourceEditing new newSource: self requestor text) signal
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> declareTempAndPaste: name [
	| insertion delta theTextString characterBeforeMark tempsMark newMethodNode |

	"Below we are getting the text that is actually seen in the morph. This is rather ugly. Maybe there is a better way to do this."
	theTextString := self requestor textMorph editor paragraph text.

	"We parse again the method displayed in the morph. The variable methodNode has the first version of the method, without temporary declarations. "
	newMethodNode := RBParser parseMethod: theTextString.

	"We check if there is a declaration of temporary variables"
	tempsMark :=  newMethodNode body  rightBar ifNil: [ self methodNode body start ].

	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].

	(theTextString at: tempsMark) = $| ifTrue:  [
  		"Paste it before the second vertical bar"
		insertion := name, ' '.

		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
		delta := 0.
	] ifFalse: [
		"No bars - insert some with CR, tab"
		insertion := '| ' , name , ' |',String cr.
		delta := 2.	"the bar and CR"
		characterBeforeMark = Character tab ifTrue: [
			insertion := insertion , String tab.
			delta := delta + 1.	"the tab" ]
		].
	tempsMark := tempsMark +
		(self substituteWord: insertion
			wordInterval: (tempsMark to: tempsMark-1)
			offset: 0) - delta.
	" we can not guess at this point where the tempvar should be stored,
	tempvars vs. tempvector therefore -> reparse"
	(ReparseAfterSourceEditing new newSource: self requestor text) signal
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> declareUndefined [

	^UndeclaredVariable registeredWithName: node name
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> defaultAction [
	| className selector |
 	className := self methodClass name.
	selector := self methodNode selector.

	NewUndeclaredWarning signal: node name in: (selector
		ifNotNil: [className, '>>', selector]
			ifNil: ['<unknown>']).

	^super defaultAction ifNil: [ self declareUndefined ]
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> defineClass: className [
	"Prompts the user to define a new class."

	| classSymbol systemCategory classDefinition classBinding result |
	classSymbol := className asSymbol.
	systemCategory := self methodClass category
		ifNil: [ 'Unknown' ].
	classDefinition := ClassDefinitionPrinter new classDefinitionTemplateInPackage: systemCategory forClass: classSymbol.
	classDefinition := UIManager default
		multiLineRequest: 'Edit class definition:'
		initialAnswer: classDefinition
		answerHeight: 200.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ Abort signal ].
	result := self class compiler
		source: classDefinition;
		logged: true;
		evaluate.
	"Because some class definition syntax the (fuild one) does not evaluate to a
	class but a class builder, we must call `fluidInstall`.
	(that is a noop on real classes)."
	result fluidInstall.
	classBinding := node owningScope lookupVar: className.
	"make sure to recompile all methods referencing this class"
	classBinding usingMethods do: [:method | method recompile].
	^classBinding
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> defineTrait: traitName [
	"Prompts the user to define a new trait."

	| traitSymbol systemCategory traitDefinition |
	traitSymbol := traitName asSymbol.
	systemCategory := self methodClass category
		ifNil: [ 'Unknown' ].
	traitDefinition := 'Trait named: #' , traitSymbol , '
		uses:{}
		package: ''' , systemCategory , ''''.
	traitDefinition := UIManager default
		multiLineRequest: 'Edit trait definition:'
		initialAnswer: traitDefinition
		answerHeight: 150.
	(traitDefinition isNil or: [ traitDefinition isEmpty ])
		ifTrue: [ ^ Abort signal ].
	self class compiler
		source: traitDefinition;
		logged: true;
		evaluate.
	^ (node owningScope lookupVar: traitSymbol)
		ifNil: [self error: 'should be not happen']
]

{ #category : #accessing }
OCUndeclaredVariableWarning >> node: aVariableNode [

	super node: aVariableNode.
	messageText := 'Undeclared temp: ', aVariableNode name
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> openMenuIn: aBlock [
	| alternatives labels actions lines caption choice name interval requestor |

	"Turn off suggestions when in RubSmalltalkCommentMode
 	This is a workaround, the plan is to not do this as part of the exception"
 	requestor := compilationContext requestor.
 	((requestor class name = #RubEditingArea) and: [
 		requestor editingMode class name = #RubSmalltalkCommentMode])
 					ifTrue: [ ^UndeclaredVariable named: node name ].

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
			actions add: [ self declareUndefined ].
			lines add: labels size.
			labels add: 'Define new class'.
			actions
				add: [
					[ self defineClass: name ]
						on: Abort
						do: [ self openMenuIn: aBlock ] ].
			labels add: 'Declare new global'.
			actions add: [ self declareGlobal ].
			compilationContext requestor isScripting ifFalse:
				[labels add: 'Declare new class variable'.
				actions add: [ self declareClassVar ]].
			labels add: 'Define new trait'.
			actions
				add: [
					[ self defineTrait: name ]
						on: Abort
						do: [ self openMenuIn: aBlock ] ] ].
	lines add: labels size.
	alternatives
		do: [ :each |
			labels add: each.
			actions
				add: [
					^self substituteVariable: each atInterval: interval ] ].
	lines add: labels size.
	labels add: 'Cancel'.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	^choice ifNotNil: [ self resume: (actions at: choice ifAbsent: [ compilationContext failBlock value ]) value ]
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> possibleVariablesFor: proposedVariable [
	| results class |
	class := node methodNode methodClass .

	results := proposedVariable correctAgainst: node methodOrBlockNode scope allTempNames
								continuedFrom: nil.
	proposedVariable isValidGlobalName ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> substituteVariable: varName atInterval: anInterval [
	self
		substituteWord: varName
		wordInterval: anInterval
		offset: 0.
	self methodNode source: self requestor text.
	node replaceWith:((RBVariableNode named: varName) binding: (node owningScope lookupVar: varName)).
	(ReparseAfterSourceEditing new newSource: self requestor text) signal.
	^ (node owningScope lookupVar: varName)
		ifNil: [self error: 'should be found']
]

{ #category : #correcting }
OCUndeclaredVariableWarning >> substituteWord: correctWord wordInterval: spot offset: o [
	"Substitute the correctSelector into the (presuamed interactive) receiver."

	self requestor correctFrom: (spot first + o)
					to: (spot last + o)
					with: correctWord.

	^ o + correctWord size - spot size
]
