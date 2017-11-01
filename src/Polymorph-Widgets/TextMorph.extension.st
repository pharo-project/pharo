Extension { #name : #TextMorph }

{ #category : #'*Polymorph-Widgets' }
TextMorph >> optimalExtent [
	"Create a new paragraph and answer its extent."
	
	^(Paragraph new
		compose: text
		style: textStyle copy
		from: 1
		in: (0@0 extent: 9999999@9999999);
		adjustRightX;
		extent) + (self borderWidth * 2) + (2@0) "FreeType kerning allowance"
]

{ #category : #'*Polymorph-Widgets' }
TextMorph >> overrideExtent: newExtent [
	"If autoFit is on then override to false for the duration of the extent call."
	
	self isAutoFit
		ifTrue: [self
				setProperty: #autoFitContents toValue: false;
				extent: newExtent;
				setProperty: #autoFitContents toValue: true]
]

{ #category : #'*Polymorph-Widgets' }
TextMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #'*Polymorph-Widgets' }
TextMorph >> wantsKeyboardFocusNavigation [
	"Answer whether the receiver wants to be navigated to.
	Answer false here (use PluggableTextMorph instead)."

	^false
]
