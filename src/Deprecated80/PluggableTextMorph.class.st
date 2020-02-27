"
A pluggable text editor
"
Class {
	#name : #PluggableTextMorph,
	#superclass : #ScrollPane,
	#instVars : [
		'textMorph',
		'getTextSelector',
		'setTextSelector',
		'getSelectionSelector',
		'hasUnacceptedEdits',
		'askBeforeDiscardingEdits',
		'selectionInterval',
		'hasEditingConflicts',
		'acceptAction',
		'getColorSelector',
		'unstyledAcceptText',
		'styler',
		'autoAccept',
		'enabled',
		'getEnabledSelector',
		'highlights',
		'acceptOnFocusChange',
		'selectionColor',
		'alwaysAccept',
		'changedAction',
		'doubleClickSelector',
		'setSelectionSelector'
	],
	#classVars : [
		'ShowTextEditingState',
		'StylingClass'
	],
	#category : #Deprecated80
}

{ #category : #deprecation }
PluggableTextMorph class >> isDeprecated [
	^ true
]

{ #category : #'instance creation' }
PluggableTextMorph class >> on: anObject text: getTextSel accept: setTextSel [

	^ self on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: nil
		menu: nil
]

{ #category : #'instance creation' }
PluggableTextMorph class >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel [

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: nil
]

{ #category : #'instance creation' }
PluggableTextMorph class >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel [

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: setSelectionSel
]

{ #category : #accessing }
PluggableTextMorph class >> shoutEnabled [
	^ (Smalltalk globals includesKey: #SHPreferences)
		and: [(Smalltalk globals at: #SHPreferences) enabled]
]

{ #category : #settings }
PluggableTextMorph class >> showTextEditingState [
	^ ShowTextEditingState ifNil: [ShowTextEditingState := true]
]

{ #category : #settings }
PluggableTextMorph class >> showTextEditingState: aBoolean [
	ShowTextEditingState := aBoolean
]

{ #category : #accessing }
PluggableTextMorph class >> stylingClass [

	^ StylingClass
]

{ #category : #accessing }
PluggableTextMorph class >> stylingClass: aClass [

	StylingClass := aClass
]

{ #category : #actions }
PluggableTextMorph >> accept [
	self acceptBasic.
	acceptAction ifNotNil: [acceptAction value: textMorph contents asText].
]

{ #category : #accessing }
PluggableTextMorph >> acceptAction [
	^acceptAction
]

{ #category : #accessing }
PluggableTextMorph >> acceptAction: anAction [
	acceptAction := anAction
]

{ #category : #actions }
PluggableTextMorph >> acceptBasic [
	"Inform the model of text to be accepted, and return true if OK."

	| ok saveSelection saveScrollerOffset |
	saveSelection := self selectionInterval copy.
	saveScrollerOffset := scroller offset copy.

	(self canDiscardEdits and: [self alwaysAccept not])
		ifTrue: [^ self flash].
 
	self hasEditingConflicts ifTrue:
		[(self confirm: 
'Caution! This method may have been
changed elsewhere since you started
editing it here.  Accept anyway?' translated) ifFalse: [^ self flash]].
	ok := self acceptTextInModel.
	ok==true 
		ifTrue: [self setText: self getText.
		self hasUnacceptedEdits: false].

	["During the step for the browser, updateCodePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step, then restores the cursor and scrollbar"

	ok==true  ifTrue: "(don't bother if there was an error during compile)"
		[(model respondsTo: #updateCodePaneIfNeeded) 
			ifTrue: [model updateCodePaneIfNeeded].
		scroller offset: saveScrollerOffset.
		self setScrollDeltas.
		self selectFrom: saveSelection first to: saveSelection last]]

			on: Error do: []

]

{ #category : #layout }
PluggableTextMorph >> acceptDroppingMorph: aMorph event: evt [ 
	"This message is sent when a morph is dropped onto a morph that has     
	agreed to accept the dropped morph by responding 'true' to the     
	wantsDroppedMorph:Event: message. The default implementation just     
	adds the given morph to the receiver."
	"Here we let the model do its work."

	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.


]

{ #category : #initialization }
PluggableTextMorph >> acceptOnCR: trueOrFalse [
	textMorph acceptOnCR: trueOrFalse
]

{ #category : #accessing }
PluggableTextMorph >> acceptOnFocusChange [
	"Answer whether the editor accepts its contents when it loses the keyboard focus."

	^acceptOnFocusChange ifNil: [false]
]

{ #category : #accessing }
PluggableTextMorph >> acceptOnFocusChange: aBoolean [
	"Set whether the editor accepts its contents when it loses the keyboard focus."

	acceptOnFocusChange := aBoolean.
	self textMorph ifNotNil: [:t | t acceptOnFocusChange: aBoolean]
]

{ #category : #actions }
PluggableTextMorph >> acceptTextInModel [ 
	"#correctFrom:to:with: is sent when the method source is
	manipulated during compilation (removing unused temps,
	changing selectors etc). But 	#correctFrom:to:with: operates 
	on the textMorph's text, and we may be saving an unstyled 
	copy of the text. This means that these corrections will be lost
	unless we also apply the corrections to the unstyled copy that we are saving.
		So remember the unstyled copy in unstyledAcceptText, so
	that when #correctFrom:to:with: is received we can also apply
	the correction to it"
	| acceptedText |
	acceptedText := self styler unstyledTextFrom: textMorph asText.
	[^setTextSelector isNil or:
		[setTextSelector numArgs = 2
			ifTrue: [model perform: setTextSelector with: acceptedText with: self]
			ifFalse: [model perform: setTextSelector with: acceptedText]]
		] ensure: [unstyledAcceptText := nil]
]

{ #category : #'menu commands' }
PluggableTextMorph >> acceptTextInModelBasic [
	"Inform the model that the receiver's textMorph's text should be accepted.
	Answer true if the model accepted ok, false otherwise"
	
	| textToAccept |
	textToAccept := textMorph asText.
	^setTextSelector isNil or:
		[setTextSelector numArgs = 2
			ifTrue: [model perform: setTextSelector with: textToAccept with: self]
			ifFalse: [model perform: setTextSelector with: textToAccept]]

]

{ #category : #polymorph }
PluggableTextMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderStyle baseColor: (self enabled ifTrue: [paneColor twiceDarker] ifFalse: [paneColor darker]).
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 
		ifTrue: [self borderStyle: self borderStyleToUse]
]

{ #category : #accessing }
PluggableTextMorph >> alwaysAccept [
	^ alwaysAccept ifNil: [false]
]

{ #category : #accessing }
PluggableTextMorph >> alwaysAccept: aBoolean [
	"Set the always accept flag."
	"This flag is used when there are unsaved changes in my text field and an exterior actor tries to modify me"
	"If the flag is true, I will accept to change even if I have pending modification instead of poping up the Accept/Discard/Cancel window"


	alwaysAccept := aBoolean

]

{ #category : #settings }
PluggableTextMorph >> appendEntry [
	"Append the text in the model's writeStream to the editable text. "
	textMorph asText size > model characterLimit ifTrue:
		["Knock off first half of text"
		self selectInvisiblyFrom: 1 to: textMorph asText size // 2.
		self replaceSelectionWith: Text new].
	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size.
	self replaceSelectionWith: model contents asText.
	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size
]

{ #category : #polymorph }
PluggableTextMorph >> appendText: aTextOrString [
	"Append the given text to the receiver."

	self handleEdit: [
		self 
			selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size;
			replaceSelectionWith: aTextOrString;
			selectFrom: textMorph asText size + 1 to: textMorph asText size;
			hasUnacceptedEdits: false;
			scrollSelectionIntoView;
			changed]
]

{ #category : #styling }
PluggableTextMorph >> askBeforeDiscardingEdits: aBoolean [
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	askBeforeDiscardingEdits := aBoolean
]

{ #category : #accessing }
PluggableTextMorph >> autoAccept [
	"Answer whether the editor accepts its contents on each change."

	^autoAccept ifNil: [false]
]

{ #category : #accessing }
PluggableTextMorph >> autoAccept: aBoolean [
	"Answer whether the editor accepts its contents on each change."

	autoAccept := aBoolean.
	self textMorph
		ifNotNil: [:t | t autoAccept: aBoolean]
]

{ #category : #styling }
PluggableTextMorph >> basicHasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "	
	aBoolean == hasUnacceptedEdits 
		ifFalse: [hasUnacceptedEdits := aBoolean. 
			self changed].
	aBoolean 
		ifFalse: [hasEditingConflicts := false].
	self okToStyle 
		ifTrue: [ self styler styleInBackgroundProcess: textMorph contents]
]

{ #category : #'menu commands' }
PluggableTextMorph >> basicInspectIt [
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result basicInspect]].
]

{ #category : #accessing }
PluggableTextMorph >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme textEditorNormalBorderStyleFor: self]
		ifFalse: [self theme textEditorDisabledBorderStyleFor: self]
]

{ #category : #'menu commands' }
PluggableTextMorph >> browseFullClass [
	self handleEdit: [textMorph editor browseFullClass]
]

{ #category : #settings }
PluggableTextMorph >> bsText [
	self changeText: (self text copyFrom: 1 to: (self text size - 1 max: 0))
]

{ #category : #'dependents access' }
PluggableTextMorph >> canDiscardEdits [
	"Return true if this view either has no text changes or does not care."

	^ (hasUnacceptedEdits and: [askBeforeDiscardingEdits]) not

]

{ #category : #'menu commands' }
PluggableTextMorph >> cancel [
	(self confirm:
'This action will cancel your changes.
Is it OK to cancel changes?' translated) ifTrue: [self setText: self getText.
	self setSelection: self getSelection.]
]

{ #category : #'menu commands' }
PluggableTextMorph >> cancelWithoutConfirmation [
	self setText: self getText.
	self setSelection: self getSelection.

]

{ #category : #'menu commands' }
PluggableTextMorph >> changeStyle [
	self handleEdit: [textMorph editor changeStyle]
]

{ #category : #settings }
PluggableTextMorph >> changeText: aText [
	"The paragraph to be edited is changed to aText."
	self setText: aText
]

{ #category : #accessing }
PluggableTextMorph >> changedAction [
	^ changedAction
]

{ #category : #accessing }
PluggableTextMorph >> changedAction: aBlock [
	changedAction := aBlock
]

{ #category : #'menu commands' }
PluggableTextMorph >> chooseAlignment [
	self handleEdit: [textMorph editor changeAlignment]
]

{ #category : #'menu commands' }
PluggableTextMorph >> classCommentsContainingIt [
	self handleEdit: [textMorph editor classCommentsContainingIt]
]

{ #category : #'menu commands' }
PluggableTextMorph >> classNamesContainingIt [
	self handleEdit: [textMorph editor classNamesContainingIt]
]

{ #category : #scrolling }
PluggableTextMorph >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"
		
	self styler classOrMetaClass: aBehavior
]

{ #category : #'unaccepted edits' }
PluggableTextMorph >> configureTextMorph: aTextMorph [
	"I prepare a text morph for use"
	aTextMorph setEditView: self.
	scroller addMorph: aTextMorph.
	aTextMorph 
		autoAccept: self autoAccept;
		selectionColor: self selectionColor.
	aTextMorph editor installKeymappingsOn: self.

]

{ #category : #'menu commands' }
PluggableTextMorph >> copySelection [
	self handleEdit: [textMorph editor copySelection]
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> correctFrom: start to: stop with: aString [
	unstyledAcceptText ifNotNil: [unstyledAcceptText replaceFrom: start to: stop with: aString ].
	^ self handleEdit: [textMorph editor correctFrom: start to: stop with: aString]
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> correctSelectionWithString: aString [
	| result newPosition |

	"I can't tell if this is a hack or if it's the right thing to do."
	self setSelection: selectionInterval. 

	result := self correctFrom: selectionInterval first to: selectionInterval last with: aString.
	newPosition := selectionInterval first + aString size.
	self setSelection: (newPosition to: newPosition - 1).
	^ result
]

{ #category : #accessing }
PluggableTextMorph >> crAction: anAction [
	self textMorph crAction: anAction
]

{ #category : #'event handling' }
PluggableTextMorph >> cursorEnd: aKeyboardEvent [ 

	^ textMorph editor cursorEnd: aKeyboardEvent.
]

{ #category : #'event handling' }
PluggableTextMorph >> cursorHome: aKeyboardEvent [ 

	^ textMorph editor cursorHome: aKeyboardEvent.
]

{ #category : #'menu commands' }
PluggableTextMorph >> cut [
	self handleEdit: [textMorph editor cut].
	self autoAccept ifTrue: [ self accept ].
]

{ #category : #'menu commands' }
PluggableTextMorph >> debugIt [
	self handleEdit: [textMorph editor debugIt]
]

{ #category : #initialization }
PluggableTextMorph >> defaultBorderWidth [
	^ 1
]

{ #category : #initialization }
PluggableTextMorph >> defaultColor [ 
	^ self theme backgroundColor
]

{ #category : #scrolling }
PluggableTextMorph >> defaultStyler [
	^ self stylerClass new view: self
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> deselect [
	^ textMorph editor deselect
]

{ #category : #actions }
PluggableTextMorph >> disable [
	"Disable the receiver."
	
	self enabled: false
]

{ #category : #'menu commands' }
PluggableTextMorph >> doIt [
	self handleEdit: [textMorph editor evaluateSelection]
]

{ #category : #'model access' }
PluggableTextMorph >> drawHighlightsOn: aCanvas [
	"Draw the highlights."
	
	|b o|
	b := self innerBounds.
	o := self scroller offset.
	aCanvas clipBy: self clippingBounds during: [:c |
	self highlights do: [:h |
		h
			drawOn: c 
			in: b
			offset: o]]
]

{ #category : #drawing }
PluggableTextMorph >> drawOn: aCanvas [
	"Indicate unaccepted edits, conflicts etc."

	super drawOn: aCanvas. 
	self wantsFrameAdornments ifTrue: [
		self theme drawTextAdornmentsFor: self on: aCanvas].
	self drawHighlightsOn: aCanvas
]

{ #category : #drawing }
PluggableTextMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Indicate unaccepted edits, conflicts etc."

	super drawOnAthensCanvas: anAthensCanvas. 
	self wantsFrameAdornments ifTrue: [
		self theme drawTextAdornmentsFor: self on: anAthensCanvas asCanvasWrapper].
	self drawHighlightsOn: anAthensCanvas asCanvasWrapper
]

{ #category : #initialization }
PluggableTextMorph >> editString: aString [ 
	"Jam some text in.  This is treated as clean text by default."

	self setText: aString asText
]

{ #category : #actions }
PluggableTextMorph >> enable [
	"Enable the receiver."
	
	self enabled: true
]

{ #category : #accessing }
PluggableTextMorph >> enabled [
	"Answer whether the receiver is enabled."
	
	^enabled ifNil: [true]
]

{ #category : #actions }
PluggableTextMorph >> enabled: aBoolean [
	"Set the value of enabled"

	enabled = aBoolean ifTrue: [^self].
	enabled := aBoolean.
	self changed: #enabled.
	self
		adoptPaneColor: self paneColor;
		changed
]

{ #category : #'menu commands' }
PluggableTextMorph >> exploreIt [
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result inspect]].
]

{ #category : #geometry }
PluggableTextMorph >> extent: newExtent [
	"The inner bounds may have changed due to scrollbar visibility."

	super extent: (newExtent max: 36 @ 16).
	textMorph
		ifNotNil: [ 
			self innerExtent = textMorph extent
				ifFalse: [ textMorph extent: self innerExtent ] ].
	self setScrollDeltas.
	(self fillStyle notNil and: [ self fillStyle isSolidFill not ])
		ifTrue: [ self fillStyle: self fillStyleToUse ]
]

{ #category : #geometry }
PluggableTextMorph >> extraScrollRange [
	^ self height // 4
]

{ #category : #'menu commands' }
PluggableTextMorph >> fileItIn [
	self handleEdit: [textMorph editor fileItIn]
]

{ #category : #accessing }
PluggableTextMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	self color ifNotNil: [ ^ self color ].
	^self enabled
		ifTrue: [self theme textEditorNormalFillStyleFor: self]
		ifFalse: [self theme textEditorDisabledFillStyleFor: self]
]

{ #category : #'menu commands' }
PluggableTextMorph >> find [
	self handleEdit: [textMorph editor find]
]

{ #category : #'menu commands' }
PluggableTextMorph >> findAgain [
	self handleEdit: [textMorph editor findAgain]
]

{ #category : #polymorph }
PluggableTextMorph >> focusBounds [
	"Answer the bounds for drawing the focus indication
	(when externalFocusForPluggableText is enabled)."

	^self theme textFocusBoundsFor: self
]

{ #category : #accessing }
PluggableTextMorph >> font [
	"Answer the probable font"

	^self textMorph
		ifNil: [TextStyle defaultFont]
		ifNotNil: [:m | m font]
]

{ #category : #initialization }
PluggableTextMorph >> font: aFont [
	textMorph beAllFont: aFont.
	self styler font: aFont
]

{ #category : #formatting }
PluggableTextMorph >> formatSourceCode [
	self
		handleEdit: [ 
			| source tree formatted |
			source := self textMorph text asString.
			tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
			formatted := tree formattedCode.
			formatted = source
				ifTrue: [ ^ self ].
			self
				editString: formatted;
				hasUnacceptedEdits: true ]
]

{ #category : #accessing }
PluggableTextMorph >> getColorSelector [
	^getColorSelector
]

{ #category : #accessing }
PluggableTextMorph >> getColorSelector: aSymbol [
	getColorSelector := aSymbol.
	self update: getColorSelector.
]

{ #category : #actions }
PluggableTextMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector
]

{ #category : #actions }
PluggableTextMorph >> getEnabledSelector: anObject [
	"Set the value of getEnabledSelector"

	getEnabledSelector := anObject.
	self updateEnabled
]

{ #category : #'model access' }
PluggableTextMorph >> getSelection [
	"Answer the model's selection interval."

	getSelectionSelector ifNil: [^1 to: 0].	"null selection"
	^model perform: getSelectionSelector withEnoughArguments: { self }
]

{ #category : #'model access' }
PluggableTextMorph >> getText [
	"Retrieve the current model text"

	| newText |
	getTextSelector ifNil: [^Text new].
	newText := model perform: getTextSelector withEnoughArguments: {self}.
	newText ifNil: [^Text new].
	^newText shallowCopy
]

{ #category : #accessing }
PluggableTextMorph >> getTextSelector [
	^getTextSelector
]

{ #category : #private }
PluggableTextMorph >> hScrollBarValue: scrollValue [
	super hScrollBarValue: scrollValue.
	textMorph ifNotNil: [textMorph selectionChanged].
	
	self triggerEvent: #hScroll with: scrollValue
]

{ #category : #private }
PluggableTextMorph >> hUnadjustedScrollRange [
"Return the width of the widest item in the list"

	textMorph ifNil: [ ^0 ].
	textMorph isWrapped ifTrue:[ ^0 ].

	^super hUnadjustedScrollRange

]

{ #category : #'editor access' }
PluggableTextMorph >> handleEdit: editBlock [
	| result |

	textMorph editor 
		selectFrom: selectionInterval first 
		to: selectionInterval last;
		model: model.  "For, eg, evaluateSelection"
	textMorph handleEdit: [result := editBlock value].   "Update selection after edit"
	self scrollSelectionIntoView.
	self textChanged.
	^ result
]

{ #category : #'event handling' }
PluggableTextMorph >> handlesDoubleClick [
	
	^ doubleClickSelector notNil
]

{ #category : #'event handling' }
PluggableTextMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #styling }
PluggableTextMorph >> hasEditingConflicts [
	"Return true if a conflicting edit to the same code (typically) is known to have occurred after the current contents started getting edited"

	^ hasEditingConflicts == true
]

{ #category : #styling }
PluggableTextMorph >> hasEditingConflicts: aBoolean [

	hasEditingConflicts := aBoolean.
	self changed
]

{ #category : #polymorph }
PluggableTextMorph >> hasKeyboardFocus [
	"Answer whether the receiver has keyboard focus."

	^super hasKeyboardFocus or: [(self textMorph ifNil: [^false]) hasKeyboardFocus]
]

{ #category : #'dependents access' }
PluggableTextMorph >> hasUnacceptedEdits [
	"Return true if this view has unaccepted edits."

	^ hasUnacceptedEdits
]

{ #category : #styling }
PluggableTextMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "
	
	(model respondsTo: #hasUnacceptedEdits:)
		ifTrue: [ model hasUnacceptedEdits: aBoolean ]
		ifFalse: [ self basicHasUnacceptedEdits: aBoolean ]
]

{ #category : #polymorph }
PluggableTextMorph >> hasValidText [
	"Return true if the text is valid for acceptance."

	^true
]

{ #category : #actions }
PluggableTextMorph >> highlights [
	"Answer the value of highlights"

	^ highlights ifNil: [#()]
]

{ #category : #actions }
PluggableTextMorph >> highlights: aCollectionOfHighlight [
	"Set the value of highlights"

	highlights := aCollectionOfHighlight
]

{ #category : #'menu commands' }
PluggableTextMorph >> implementorsOfIt [
	self handleEdit: [textMorph editor implementorsOfIt]
]

{ #category : #initialization }
PluggableTextMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	
	hasUnacceptedEdits := false.
	hasEditingConflicts := false.
	askBeforeDiscardingEdits := true.
	enabled := true.
	highlights := OrderedCollection new
]

{ #category : #geometry }
PluggableTextMorph >> innerExtent [
	^ self innerBounds extent - 6
]

{ #category : #'menu commands' }
PluggableTextMorph >> inspectIt [
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result inspect]]
]

{ #category : #testing }
PluggableTextMorph >> isAutoFit [
	^textMorph isAutoFit
]

{ #category : #'event handling' }
PluggableTextMorph >> keyStroke: evt [
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	self eventHandler ifNotNil:
		[self eventHandler keyStroke: evt fromMorph: self].

]

{ #category : #polymorph }
PluggableTextMorph >> keyboardFocusChange: aBoolean [
	"Pass on to text morph."
	super keyboardFocusChange: aBoolean.
	aBoolean ifTrue: [self textMorph takeKeyboardFocus]
]

{ #category : #'event handling' }
PluggableTextMorph >> keystrokeFromTextMorph: anEvent [
	self eventHandler ifNotNil:
		[^ self eventHandler keyStroke: anEvent fromMorph: self].
	^ false
]

{ #category : #polymorph }
PluggableTextMorph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	
	super layoutBounds: aRectangle.
	textMorph ifNotNil:
		[textMorph extent: (self innerExtent x)@self height].
	self setScrollDeltas

]

{ #category : #'menu commands' }
PluggableTextMorph >> methodNamesContainingIt [
	self handleEdit: [textMorph editor methodNamesContainingIt]
]

{ #category : #'menu commands' }
PluggableTextMorph >> methodSourceContainingIt [
	self handleEdit: [textMorph editor methodSourceContainingIt]
]

{ #category : #'menu commands' }
PluggableTextMorph >> methodStringsContainingit [
	self handleEdit: [textMorph editor methodStringsContainingit]
]

{ #category : #polymorph }
PluggableTextMorph >> minHeight [
	"Implemented here since extent: overriden."

	^super minHeight max: 16
]

{ #category : #polymorph }
PluggableTextMorph >> minWidth [
	"Implemented here since extent: overriden."

	^super minWidth max: 36
]

{ #category : #initialization }
PluggableTextMorph >> model: aModel [
	"Update the enablement state too."
	super model: aModel.
	self updateEnabled
]

{ #category : #'event handling' }
PluggableTextMorph >> mouseDownFromTextMorph: anEvent [
	"Nothing to do here normally"
]

{ #category : #'event handling' }
PluggableTextMorph >> mouseEnter: event [
	"Changed to take keyboardFocusOnMouseDown into account."
	
	super mouseEnter: event.
	self textMorph ifNil: [^self].
	selectionInterval ifNotNil:
		[self textMorph editor selectInterval: selectionInterval; setEmphasisHere].
	self textMorph selectionChanged.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown
		ifFalse: [self textMorph takeKeyboardFocus]
]

{ #category : #'event handling' }
PluggableTextMorph >> mouseLeave: event [
	"The mouse has left the area of the receiver"

	textMorph ifNotNil: [selectionInterval := textMorph editor selectionInterval].
	super mouseLeave: event.
	self mouseOverForKeyboardFocus 
		ifTrue:	[event hand releaseKeyboardFocus: textMorph]
]

{ #category : #polymorph }
PluggableTextMorph >> navigationKey: event [
	"Check for tab key activity and change focus as appropriate.
	Must override here rather than in #tabKey: otherwise
	the tab will get passed to the window and change the focus."

	(event keyCharacter = Character tab and: [
		(event anyModifierKeyPressed or: [event shiftPressed]) not]) ifTrue: [^false].
	^super navigationKey: event
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> nextTokenFrom: start direction: dir [
	^ textMorph nextTokenFrom: start direction: dir
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> notify: aString at: anInteger in: aStream [
	^ textMorph editor notify: aString at: anInteger in: aStream
]

{ #category : #'menu commands' }
PluggableTextMorph >> offerFontMenu [
	self handleEdit: [textMorph editor changeTextFont]
]

{ #category : #scrolling }
PluggableTextMorph >> okToStyle [
	self shoutEnabled 
		ifFalse: [^ false].
	(model respondsTo: #shoutAboutToStyle:) 
		ifFalse: [^false].
	^model shoutAboutToStyle: self

]

{ #category : #initialization }
PluggableTextMorph >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel [

	self on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: nil.

]

{ #category : #initialization }
PluggableTextMorph >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel [

	self model: anObject.
	getTextSelector := getTextSel.
	setTextSelector := setTextSel.
	getSelectionSelector := getSelectionSel.
	setSelectionSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	self borderWidth: 1.
	self setText: self getText.
	self setSelection: self getSelection.
]

{ #category : #'event handling' }
PluggableTextMorph >> onKeyStrokeSend: sel to: recipient [
	textMorph on: #keyStroke send: sel to: recipient.
]

{ #category : #'menu commands' }
PluggableTextMorph >> paste [
	self handleEdit: [textMorph editor paste].
	self autoAccept ifTrue: [ self accept ].
]

{ #category : #'menu commands' }
PluggableTextMorph >> pasteRecent [
	"Paste an item chosen from RecentClippings."

	| clipping |
	(clipping := Clipboard chooseRecentClipping) ifNil: [^ self].
	Clipboard clipboardText: clipping.
	self paste.
]

{ #category : #'menu commands' }
PluggableTextMorph >> prettyPrint [
	self handleEdit: [textMorph editor prettyPrint]
]

{ #category : #'menu commands' }
PluggableTextMorph >> printIt [
	| oldEditor printString |
	textMorph editor
		selectFrom: selectionInterval first to: selectionInterval last;
		model: model.	"For, eg, evaluateSelection"
	textMorph
		handleEdit: [ 
			(oldEditor := textMorph editor)
				evaluateSelectionAndDo: [ :result | 
					printString := [ result printString ]
						on: Error
						do: [ '<error in printString: try ''Inspect it'' to debug>' ].
					selectionInterval := oldEditor selectionInterval.
					textMorph installEditorToReplace: oldEditor.
					textMorph handleEdit: [ oldEditor afterSelectionInsertAndSelect: printString ].
					selectionInterval := oldEditor selectionInterval.
					textMorph editor selectFrom: selectionInterval first to: selectionInterval last.
					self scrollSelectionIntoView ] ]
]

{ #category : #'menu commands' }
PluggableTextMorph >> printerSetup [
	self handleEdit: [textMorph editor printerSetup]
]

{ #category : #styling }
PluggableTextMorph >> promptForCancel [
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := ( UIManager default
		confirm: 'Code has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil ).
	choice ifNotNil: [
		choice
			ifTrue: [ self accept ]
			ifFalse: [ self model clearUserEditFlag ]]
]

{ #category : #'menu commands' }
PluggableTextMorph >> redo [
	self handleEdit: [ textMorph editor redo ].
	self autoAccept ifTrue: [ self accept ].
]

{ #category : #'menu commands' }
PluggableTextMorph >> referencesToIt [
	self handleEdit: [textMorph editor referencesToIt]
]

{ #category : #actions }
PluggableTextMorph >> registerScrollChanges: aSelector [

	self announcer 
		when: PaneScrolling 
		send: aSelector
		to: self model
]

{ #category : #settings }
PluggableTextMorph >> replaceSelectionWith: aText [
	^ textMorph editor replaceSelectionWith: aText
]

{ #category : #geometry }
PluggableTextMorph >> resetExtent [
	"Reset the extent while maintaining the current selection.  Needed when resizing while the editor is active (when inside the pane)."
	| tempSelection |
	textMorph notNil ifTrue:
		["the current selection gets munged by resetting the extent, so store it"
		tempSelection := self selectionInterval.
		super resetExtent. "adjust scroller"
		self extent: self extent.
		self setSelection: tempSelection]
]

{ #category : #updating }
PluggableTextMorph >> scrollBarMenuButtonPressed: event [
	"The menu button in the scrollbar was pressed; put up the menu"

	| menu |
	(menu := self getMenu: event shiftPressed) ifNotNil:
		["Set up to use perform:orSendTo: for model/view dispatch"
		menu setInvokingView: self.
		menu invokeModal]
]

{ #category : #private }
PluggableTextMorph >> scrollBy: delta [
	"Move the contents in the direction delta."
	
	super scrollBy: delta.
	
	self triggerEvent: #hScroll with: self scrollValue x.
	self triggerEvent: #vScroll with: self scrollValue y
	
]

{ #category : #geometry }
PluggableTextMorph >> scrollDeltaHeight [
	"Return the increment in pixels which this pane should be scrolled."
	^ scroller firstSubmorph defaultLineHeight

]

{ #category : #'editor access' }
PluggableTextMorph >> scrollSelectionIntoView [
	"Scroll my text into view if necessary and return true, else return false"
	^ self scrollSelectionIntoView: nil
]

{ #category : #'editor access' }
PluggableTextMorph >> scrollSelectionIntoView: event [ 
	"Scroll my text into view if necessary and return true, else return false"
	| selRects delta selRect rectToTest transform cpHere editor |
	editor := textMorph editor.
	self selectionInterval: editor selectionInterval.
	selRects := textMorph paragraph selectionRects.
	selRects isEmpty ifTrue: [^ false].
	rectToTest := selRects first merge: selRects last.
	transform := scroller transformFrom: self.
	(event notNil and: [event anyButtonPressed]) ifTrue:  "Check for autoscroll"
		[cpHere := transform localPointToGlobal: event cursorPoint.
		cpHere y <= self top
			ifTrue: [rectToTest := selRects first topLeft extent: 2@2]
			ifFalse: [cpHere y >= self bottom
					ifTrue: [rectToTest := selRects last bottomRight extent: 2@2]
					ifFalse: [^ false]]].
	selRect := transform localBoundsToGlobal: rectToTest.
	selRect height > bounds height
		ifTrue: [(editor pointIndex - editor markIndex) < 0
			ifTrue: [self scrollBy: 0@(self innerBounds top - selRect top)]
			ifFalse: [self scrollBy: 0@(self innerBounds bottom - selRect bottom)].
		^ true].
	(delta := selRect amountToTranslateWithin: self innerBounds) y ~= 0 ifTrue:
		["Scroll end of selection into view if necessary"
		self scrollBy: 0@delta y.
		^ true].
	^ false

]

{ #category : #scrolling }
PluggableTextMorph >> scrollToBottom [
	"Scroll to the bottom."
	
	self
		vScrollBarValue: self vTotalScrollRange;
		setScrollDeltas
]

{ #category : #actions }
PluggableTextMorph >> scrollToTop [
	"Scroll to the top."
	
	self
		vScrollBarValue: 0;
		setScrollDeltas
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> select [
	^ textMorph editor select
]

{ #category : #'editor access' }
PluggableTextMorph >> selectAll [
	"Tell my textMorph's editor to select all"

	self textMorph editor selectAll.
	self selectionInterval: self textMorph editor selectionInterval
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> selectFrom: start to: stop [
	self textMorph editor selectFrom: start to: stop.
	self selectionInterval: self textMorph editor selectionInterval.
	^ self selectionInterval
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> selectInvisiblyFrom: start to: stop [
	^ textMorph editor selectInvisiblyFrom: start to: stop
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> selectedContents [
	^ textMorph editor selection
]

{ #category : #accessing }
PluggableTextMorph >> selectionColor [
	"Answer the color to use for the text selection."

	^ selectionColor 
]

{ #category : #accessing }
PluggableTextMorph >> selectionColor: aColor [
	"Set the colour to use for the text selection."

	selectionColor := aColor.
	self textMorph 
		ifNotNil: [:t | t selectionColor: aColor]
]

{ #category : #'interactive error protocol' }
PluggableTextMorph >> selectionInterval [
	^ textMorph editor selectionInterval
]

{ #category : #'model access' }
PluggableTextMorph >> selectionInterval: sel [
	selectionInterval := sel.
	setSelectionSelector ifNotNil: [ model perform:setSelectionSelector with: sel ].
]

{ #category : #'menu commands' }
PluggableTextMorph >> sendersOfIt [
	self handleEdit: [textMorph editor sendersOfIt]
]

{ #category : #actions }
PluggableTextMorph >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]

{ #category : #'menu commands' }
PluggableTextMorph >> setSearchString [
	self handleEdit: [textMorph editor setSearchString]
]

{ #category : #'model access' }
PluggableTextMorph >> setSelection: sel [
	self selectionInterval: sel.
	textMorph editor selectFrom: sel first to: sel last.
	self scrollSelectionIntoView ifFalse: [scroller changed].
]

{ #category : #accessing }
PluggableTextMorph >> setSelectionSelector [

	^ setSelectionSelector
]

{ #category : #accessing }
PluggableTextMorph >> setSelectionSelector: aSelector [

	setSelectionSelector := aSelector

	
]

{ #category : #'model access' }
PluggableTextMorph >> setText: aText [
	self okToStyle
		ifFalse: [ self setTextBasic: aText ]
		ifTrue: [
			self setTextBasic: (self styler format: aText).
			aText size < 4096
				ifTrue: [ self styler style: textMorph contents ]
				ifFalse: [ self styler styleInBackgroundProcess: textMorph contents ] ]
]

{ #category : #'model access' }
PluggableTextMorph >> setTextBasic: aText [

	((self textMorph contents size = 0) and: [self wrapFlag])
		ifTrue: [ 
			self textMorph
				contents: aText 
				wrappedTo: self innerExtent x ]
		ifFalse: [
			self textMorph newContents: aText].
	
	self hasUnacceptedEdits: false.
	self setScrollDeltas.
]

{ #category : #'editor access' }
PluggableTextMorph >> setTextMorphToSelectAllOnMouseEnter [
	"Tell my textMorph's editor to select all when the mouse enters"

	textMorph on: #mouseEnter send: #selectAll to: textMorph
]

{ #category : #actions }
PluggableTextMorph >> setViewToFirstHighlight [
	| min lines |
	
	min := (self highlights detectMin: [:e | e lineNumber ]) lineNumber.
	lines := self textMorph paragraph lines size.
	
	self vScrollValue: (min/lines)
]

{ #category : #scrolling }
PluggableTextMorph >> shoutEnabled [
	^ self class shoutEnabled
]

{ #category : #'scroll bar events' }
PluggableTextMorph >> showTextEditingState [
	^ self class showTextEditingState
]

{ #category : #accessing }
PluggableTextMorph >> smartSuggestions [
	self handleEdit: [textMorph editor smartSuggestions]
]

{ #category : #'drag and drop' }
PluggableTextMorph >> startDrag: event [
	| aTransferMorph itemMorph passenger |
	
	self dragEnabled ifFalse: [ ^ self ].

	itemMorph := self selectedContents asMorph.
	
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
				"Ask the draggedMorph otherwise the transferMorph has not yet its bounds"
			aTransferMorph align: aTransferMorph draggedMorph center with: event position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			event hand grabMorph: aTransferMorph].
	event hand releaseMouseFocus: self
]

{ #category : #scrolling }
PluggableTextMorph >> styled: aBoolean [

	self styler stylingEnabled: aBoolean

]

{ #category : #scrolling }
PluggableTextMorph >> styler [
	"The styler responsible for highlighting text in the receiver"
	^ styler ifNil: [styler := self defaultStyler]
]

{ #category : #scrolling }
PluggableTextMorph >> styler: aStyler [
	"Set the styler responsible for highlighting text in the receiver"
	styler := aStyler
]

{ #category : #accessing }
PluggableTextMorph >> stylerClass [
	^ self class stylingClass ifNil: [NullTextStyler]
]

{ #category : #accessing }
PluggableTextMorph >> stylerStyled: styledCopyOfText [ 

	textMorph contents runs: styledCopyOfText runs.
	textMorph updateFromParagraph.
	selectionInterval
		ifNotNil: [textMorph editor selectInvisiblyFrom: selectionInterval first to: selectionInterval last;
				 storeSelectionInParagraph;
				 setEmphasisHere].
	textMorph editor blinkParen.
	self scrollSelectionIntoView
]

{ #category : #accessing }
PluggableTextMorph >> stylerStyledInBackground: styledCopyOfText [ 
	textMorph contents string = styledCopyOfText string
		ifTrue: [self stylerStyled: styledCopyOfText]
]

{ #category : #polymorph }
PluggableTextMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #'menu commands' }
PluggableTextMorph >> tallyIt [
	self handleEdit: [textMorph editor tallyIt]
]

{ #category : #'model access' }
PluggableTextMorph >> text [
	^ textMorph contents
]

{ #category : #'event handling' }
PluggableTextMorph >> textChanged [
	"this is used in the case where we want to listen for live changes in a text morph. otherwise we're limited to the accept behavior"
	
	changedAction ifNotNil: [changedAction value: textMorph contents asText].
]

{ #category : #'model access' }
PluggableTextMorph >> textColor: aColor [
	"Set the color of my text to the given color"

	self textMorph textColor: aColor
]

{ #category : #actions }
PluggableTextMorph >> textExtent [
	"Answer the text morph extent."
	
	^(textMorph ifNil: [^0@0]) extent
]

{ #category : #actions }
PluggableTextMorph >> textExtent: newExtent [
	"If autoFit is on then override to false for the duration of the extent call."
	
	textMorph ifNil: [^self].
	textMorph overrideExtent: newExtent
]

{ #category : #'editor access' }
PluggableTextMorph >> textMorph [
	^ textMorph ifNil: [ 
		self textMorph: self textMorphClass new.
		textMorph ]
]

{ #category : #'editor access' }
PluggableTextMorph >> textMorph: aTextMorph [
	textMorph ifNotNil: [ 
		scrollBar removeMorph: textMorph ].
	textMorph := aTextMorph.
	self configureTextMorph: textMorph.
]

{ #category : #'unaccepted edits' }
PluggableTextMorph >> textMorphClass [
	"Answer the class used to create the receiver's textMorph"
	
	^TextMorphForEditView 
]

{ #category : #polymorph }
PluggableTextMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]

{ #category : #'editor access' }
PluggableTextMorph >> toggleCommentOnSelectionOrLine [
	"Tell my textMorph's editor to comment or uncomment current selection or line if no there is no selection"

	textMorph editor toggleCommentOnSelectionOrLine
	
]

{ #category : #'menu commands' }
PluggableTextMorph >> undo [
	self handleEdit: [textMorph editor undo].
	self autoAccept ifTrue: [ self accept ].
]

{ #category : #transcript }
PluggableTextMorph >> update: aSymbol [ 
	aSymbol ifNil: [^self].
	(aSymbol == getColorSelector)
		ifTrue: [^ self color: (model perform: getColorSelector)].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == getTextSelector 
		ifTrue: 
			[self setText: self getText.
			^self setSelection: self getSelection].
	aSymbol == getSelectionSelector 
		ifTrue: [^self setSelection: self getSelection].
	aSymbol == #clearUserEdits ifTrue: [^self hasUnacceptedEdits: false].
	aSymbol == #wantToChange 
		ifTrue: 
			[self canDiscardEdits ifFalse: [^self promptForCancel].
			^self].
	aSymbol == #appendEntry 
		ifTrue: 
			[self handleEdit: [self appendEntry].
			^self ].
	aSymbol == #clearText 
		ifTrue: 
			[self handleEdit: [self changeText: Text new].
			^self ].
	aSymbol == #bs 
		ifTrue: 
			[self handleEdit: [self bsText].
			^self ].
	aSymbol == #codeChangedElsewhere 
		ifTrue: 
			[self hasEditingConflicts: true.
			^self].
	aSymbol == self getEnabledSelector 
		ifTrue: [self updateEnabled].
	^ self
]

{ #category : #actions }
PluggableTextMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]

{ #category : #private }
PluggableTextMorph >> vScrollBarValue: scrollValue [
	super vScrollBarValue: scrollValue.
	textMorph ifNotNil: [textMorph selectionChanged].
	self triggerEvent: #vScroll with: scrollValue
]

{ #category : #'dropping/grabbing' }
PluggableTextMorph >> wantsDroppedMorph: aMorph event: anEvent [ 
	^ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self
]

{ #category : #actions }
PluggableTextMorph >> wantsFrameAdornments [
	"Answer whether the receiver wishes to have red borders, etc.,  
	used to show editing state"
	"A 'long-term temporary workaround': a nonmodular,  
	unsavory, but expedient way to get the desired effect, sorry.  
	Clean up someday."
	^ self
		valueOfProperty: #wantsFrameAdornments
		ifAbsent: [self showTextEditingState
					ifTrue: [(#(searchString infoViewContents ) includes: getTextSelector) not]
					ifFalse: [false]]
]

{ #category : #drawing }
PluggableTextMorph >> wantsFrameAdornments: aBoolean [ 
	self setProperty: #wantsFrameAdornments toValue: aBoolean
]

{ #category : #polymorph }
PluggableTextMorph >> wantsKeyboardFocus [
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally). Even if disabled
	we allow for text morphs since can potentially copy text."

	^self takesKeyboardFocus and: [
		self visible and: [
			self enabled or: [self valueOfProperty: #wantsKeyboardFocusWhenDisabled ifAbsent: [true]]]]

]

{ #category : #actions }
PluggableTextMorph >> wrapFlag [
	"Answer the wrap flag on the text morph."
	
	^self textMorph wrapFlag
]

{ #category : #accessing }
PluggableTextMorph >> wrapFlag: aBoolean [
	self textMorph ifNil: [self setText: ''].
	textMorph wrapFlag: aBoolean
]

{ #category : #'menu commands' }
PluggableTextMorph >> yellowButtonActivity [
	"Called when the shifted-menu's 'more' item is chosen"
	^ self yellowButtonActivity: false
]

{ #category : #'menu commands' }
PluggableTextMorph >> yellowButtonActivity: shiftKeyState [ 
	"Called when the shifted-menu's 'more' item is chosen"
	
	(self getMenu: shiftKeyState)
		ifNotNil: [ :menu|
			menu setInvokingView: self.
			menu invokeModal.
			^ true].
	^ false
]
