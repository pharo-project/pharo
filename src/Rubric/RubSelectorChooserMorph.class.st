Class {
	#name : #RubSelectorChooserMorph,
	#superclass : #IdentifierChooserMorph,
	#instVars : [
		'prefix',
		'requestorPos'
	],
	#category : #'Rubric-Editing-Core-Support'
}

{ #category : #opening }
RubSelectorChooserMorph class >> popUpPrefix: aPrefix from: aTextMorph [ 
	Cursor wait
		showWhile: [| selectors chooser |
			selectors := self selectorsFromPrefix: aPrefix.
			selectors size = 1
				ifTrue: [aTextMorph editor selectorChooserChosenSelector: selectors first.
					^ nil].
			selectors isEmpty
				ifFalse: [chooser := self new
								chooseBlock: [:chosen | aTextMorph editor selectorChooserChosenSelector: chosen];
								 requestor: aTextMorph;
								 prefix: aPrefix; 
								 labels: selectors;
								 yourself.
					chooser open].
			^ chooser]
]

{ #category : #accessing }
RubSelectorChooserMorph class >> selectorsFromPrefix: aPrefix [
	"returns the value of labels from a selector prefix"
	"self selectorsFromPrefix: 'prin' "
	| key applicants |
	aPrefix isEmptyOrNil  ifTrue: [^ #()].
	applicants := aPrefix first canBeGlobalVarInitial
		ifTrue: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial])
					ifTrue: [strm nextPut: each]]]]
		ifFalse: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial not])
					ifTrue: [strm nextPut: each]]]].
	key := aPrefix asLowercase.
	^ (applicants select: [:each | (each asLowercase beginsWith: key) and: [each noneSatisfy: [:c | c tokenish not]]]) sort

]

{ #category : #action }
RubSelectorChooserMorph >> choose: index [
	"Set the given choice and ok."
	requestor ifNil: [^ self delete].
	requestor editor selectorChooser ~= self 
		ifTrue: [^ self delete].
	requestor takeKeyboardFocus.
	super choose: index
	
]

{ #category : #action }
RubSelectorChooserMorph >> close [
	requestor 
		ifNotNil: [requestor takeKeyboardFocus.
			self choose: 0]
]

{ #category : #'submorphs-add/remove' }
RubSelectorChooserMorph >> comeToFront [
	super comeToFront.
	self setPosition 
]

{ #category : #accessing }
RubSelectorChooserMorph >> defaultBaseColor [
	^ (self theme menuColorFor: self) muchLighter
]

{ #category : #drawing }
RubSelectorChooserMorph >> drawCommonPrefixAreasOn: aCanvas [

	requestor ifNil: [^ self ].
	prefix  
		ifNotNil: [ self choiceMenus 
			do: [:cm | self drawCommonPrefixAreasOn: aCanvas menu: cm]]

]

{ #category : #drawing }
RubSelectorChooserMorph >> drawCommonPrefixAreasOn: aCanvas menu: aMenu [
	| firstMenuItem topLeft bottomLeft lastMenuItem w c |
	firstMenuItem := aMenu menuItems first.
	lastMenuItem := aMenu menuItems last.
	w := (firstMenuItem fontToUse widthOfString: prefix).
	topLeft := firstMenuItem boundsInWorld topLeft.
	bottomLeft := lastMenuItem boundsInWorld bottomLeft + ( w @ 0 ).
	c := aMenu color muchDarker.
	aCanvas fillRectangle: (topLeft  corner: bottomLeft) color: (c alpha: 0.3)
]

{ #category : #drawing }
RubSelectorChooserMorph >> fullDrawOn: aCanvas [
	super fullDrawOn: aCanvas.
	self drawCommonPrefixAreasOn: aCanvas
	
]

{ #category : #'ui-building' }
RubSelectorChooserMorph >> newContentMorph [ 
	| sp |
	sp := super newContentMorph.
	sp
		updateScrollbars.
	^ sp

]

{ #category : #'ui-building' }
RubSelectorChooserMorph >> nextPrefixWith: aPrefix [
	| curr pref found lab |
	curr := aPrefix size.
	lab := self labels first.
	found := false.
	[lab size <= curr 
		ifTrue: [found := true]
		ifFalse: [pref := lab copyFrom: 1 to: curr.
			self labels 
				do: [:l | found 
					ifFalse: [(l asString beginsWith: pref)
						ifFalse: [pref := pref allButLast.
							found := true]]]].
	found] whileFalse: [curr := curr + 1].
	^ (pref notNil and: [pref size > aPrefix size])
		ifTrue: [pref]

]

{ #category : #'ui-building' }
RubSelectorChooserMorph >> open [
	super open. 
	self comeToFront.
	self borderStyle: ( self theme progressBarBorderStyleFor: self).
	self setPosition
]

{ #category : #'ui-building' }
RubSelectorChooserMorph >> positionInRequestor [
	|  caret  editor pos allowedArea |
	requestor ifNil: [^self].
	editor := requestor editor.
	caret := editor stopIndex.
	editor selectAt: caret.
	pos := editor selectionPosition: prefix.
	allowedArea := self allowedArea.
	(pos x + self width) > allowedArea width
		ifTrue: [pos := pos - (self width @ 0) - ((50 + 40)@ 0)].
	(pos y + self height) > allowedArea height
		ifTrue: [pos := (pos x @ (allowedArea height - self height - 5))].
	^ pos 
 
]

{ #category : #accessing }
RubSelectorChooserMorph >> prefix: aPrefix [
	"Set the value of labels"
	prefix := aPrefix.

]

{ #category : #'ui-building' }
RubSelectorChooserMorph >> setPosition [
	self position: self positionInRequestor.

]

{ #category : #stepping }
RubSelectorChooserMorph >> step [
	[requestorPos 
		ifNil: [requestorPos := requestor owner position]
		ifNotNil: [requestorPos ~= requestor owner position
			ifTrue: [^ self close]].
	(requestor isNil or: [requestor isInWorld not])
		ifTrue: [^ self close].
	requestor editor selectorChooser ~= self 
		ifTrue: [self close]
	] on: Error do: [self delete]

]

{ #category : #stepping }
RubSelectorChooserMorph >> stepTime [
	^ 500
]

{ #category : #stepping }
RubSelectorChooserMorph >> wantsSteps [ 
	^ requestor notNil
]
