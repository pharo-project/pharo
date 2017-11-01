"
I'm a MorphTreeModel enriched with a filter text field. When a pattern is entered in the textfield, then only appropriate root items are drawn in the tree. Very useful to filter a list of items. This model should be used everywhere a filter is added on top of a list as in Nautilus

Examples:
	""An example with auto-accept for the filter 
	(accepted as you type is the default)""
	| model morph |
	model := FilteredMorphTreeModel new.
	model rootItems: Morph allSubclasses.
	morph :=  model defaultMorphIn: World.
	morph extent: 300@500.
	morph openInWorld
	
	""Another example with a ghost string, and the user 
	has to accept the filter contents explicitly 
	with a return  or CMD-S in the text field""
	| model morph |
	model := FilteredMorphTreeModel new 
		ghostString: 'Enter a pattern'; 
		autoAccept: false; 
		yourself.
	model rootItems: Morph allSubclasses.
	morph :=  model defaultMorphIn: World.
	morph extent: 300@500.
	morph openInWorld
	
	""A dialog window with a filtered list""
	| window m |
	window := StandardWindow new model: self.
	window title: 'Test runner'.
	m := FilteredMorphTreeModel new
		wrapBlockOrSelector: #selector;
		rootItems: Morph methods.
	window addMorph: (m defaultMorphIn: window) fullFrame: LayoutFrame identity.
	window openInWorld

Internal Representation and Key Implementation Points.
The initial root item list is kept locally.
See #rootItems:

Instance Variables
	ghostString:		<String>
	autoAccept:		<Boolean>
	initialItems:		<Collection>
	patternModel:		<RubScrolledTextModel>
"
Class {
	#name : #FilteredMorphTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'patternModel',
		'ghostString',
		'initialRootItems',
		'autoAccept'
	],
	#category : 'Morphic-Widgets-Tree'
}

{ #category : #example }
FilteredMorphTreeModel class >> example [
	"self example"

	| window row m |
	window := StandardWindow new model: self.
	window title: 'Test runner'.
	row := (m := self new)
		wrapBlockOrSelector: #selector;
		rootItems: Morph methods;
		defaultMorphIn: window.
	m updateList.
	window addMorph: row fullFrame: LayoutFrame identity.
	window openInWorld
]

{ #category : #accessing }
FilteredMorphTreeModel >> autoAccept: aBoolean [
	"If true (default) the filter acts immediately on the list, else, the user has to enter a carriage return or to accept with cms-s"
	autoAccept := aBoolean
]

{ #category : #view }
FilteredMorphTreeModel >> defaultMorphIn: aThemable [
	| treeMorph patternMorph |
	treeMorph := self defaultTreeMorph buildContents.
	patternMorph := patternModel newTextField
		hResizing: #spaceFill;
		autoAccept: autoAccept;
		withGhostText: ghostString;
		textFont: StandardFonts defaultFont;
		yourself.
	patternMorph ghostTextRuler font: StandardFonts defaultFont.
	patternMorph  announcer when: RubReturnEntered  send: #whenReturnInFilter: to: self.
	^ self theme
		newColumnIn: aThemable
		for: {patternMorph. treeMorph}
]

{ #category : #'accessing pattern' }
FilteredMorphTreeModel >> ensureSafePattern: aPattern [
	" do not throw an error if the pattern is bad - important in case of auto-accepting"
	^ [ aPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | ]
]

{ #category : #accessing }
FilteredMorphTreeModel >> ghostString [
	^ ghostString
]

{ #category : #accessing }
FilteredMorphTreeModel >> ghostString: aString [
	"set the help text that is displayed in the filter text field when empty"
	ghostString := aString
]

{ #category : #accessing }
FilteredMorphTreeModel >> initialRootItems [
	"contains the full list, the filtered list is in rootItems"
	^ initialRootItems ifNil: [ initialRootItems := #() ]
]

{ #category : #initialization }
FilteredMorphTreeModel >> initialize [
	super initialize.
	patternModel := RubScrolledTextModel new.
	patternModel announcer 
		when: RubTextUpdatedInModel 
		send: #whenPatternChanged: 
		to: self.
	autoAccept := true.
	ghostString := ''
]

{ #category : #'accessing pattern' }
FilteredMorphTreeModel >> pattern [
	" the pattern to use to filter the list"
	^ self ensureSafePattern: patternModel getText asString trimBoth

]

{ #category : #accessing }
FilteredMorphTreeModel >> patternModel [
	"The filter model"
	^ patternModel
]

{ #category : #accessing }
FilteredMorphTreeModel >> rootItems: aListOfItems [
	"set the root items as in superclass with a copy + keep the list locally"
	initialRootItems := aListOfItems.
	super rootItems: initialRootItems copy.
	self updateList
]

{ #category : #'accessing pattern' }
FilteredMorphTreeModel >> updateList [
	"update the list by applying the pattern on the initial root item list"
	super rootItems: (self initialRootItems select: [ :p | (self pattern matchesIn: p name) notEmpty ]).
	super updateList
]

{ #category : #'accessing pattern' }
FilteredMorphTreeModel >> whenPatternChanged: anAnnouncement [
	"just update the list"
	self updateList
]

{ #category : #'accessing pattern' }
FilteredMorphTreeModel >> whenReturnInFilter: anAnnouncement [ 
	"needed if auto-accept is false"
	patternModel acceptEditsInView  
]
