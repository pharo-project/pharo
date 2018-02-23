"
RubTextAreaExamples blueTextOnGrayBacgroundNotWrapped.
RubTextAreaExamples blueTextOnGrayBacgroundWrappedTo400.
RubTextAreaExamples blueTextOnGrayBacgroundWrappedTo400Version2.
RubTextAreaExamples profileSourcesFileViewing.
RubTextAreaExamples rightFlushed.
RubTextAreaExamples shoutedScriptingTextArea.
RubTextAreaExamples simplestEmptyNotWrapped.
RubTextAreaExamples simplestEmptyWrappedTo400.
RubTextAreaExamples simplestWrappedTo400.
RubTextAreaExamples smalltalkCodeWrappedTo400.
RubTextAreaExamples sourceCodeTemplateWrappedTo400.
RubTextAreaExamples smalltalkCodeWrappedWithUnderlineSegments.
RubTextAreaExamples textFieldWithCRAction.
RubTextAreaExamples withOnlyThePrimarySelection.
RubTextAreaExamples wrappedTo400WithMarginsOf40.
"
Class {
	#name : #RubTextAreaExamples,
	#superclass : #Object,
	#classVars : [
		'ModelForShout'
	],
	#category : #'Rubric-Editing-Core-Examples'
}

{ #category : #examples }
RubTextAreaExamples class >> autoReactingText [
	"self autoReactingText"

	| area seg blk |
	area := RubEditingArea new
		beWrapped;
		withoutSelectionBar.
	area setTextWith: 'Click or keep the mouse down on [0]'.
	area width: 400.
	seg := RubPlainSegmentMorph inTextArea: area from: 34 to: 35.
	blk := [ seg replaceTextWith: (seg getText string asNumber + 1) asString ].
	seg on: #mouseDown send: #value to: blk.
	seg on: #mouseStillDown send: #value to: blk.
	seg beExtendable.
	area addSegment: seg.
	area openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> blueTextOnGrayBacgroundNotWrapped [
	"self blueTextOnGrayBacgroundNotWrapped"

	RubEditingArea new
		backgroundColor: Color lightGray;
		updateTextWith: self smallLoremIpsum;
		textColor: Color blue;
		beNotWrapped; 
		width: 400;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> blueTextOnGrayBacgroundWrappedTo400 [
	"self blueTextOnGrayBacgroundWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: self smallLoremIpsum;
		backgroundColor: Color lightGray;
		textColor: Color blue;
		font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> blueTextOnGrayBacgroundWrappedTo400Version2 [
	"self blueTextOnGrayBacgroundWrappedTo400Version2"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: ((Text fromString: self smallLoremIpsum) addAttribute: (TextColor color: Color blue));
		backgroundColor: Color lightGray;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> centered [
	"self centered"

	RubEditingArea new
		beWrapped;
		width: 300;
		updateTextWith: 'AAAAAA' , Character cr asString , 'BBBB';
		selectFrom: 5 to: 8;
		centered;
		openInWorld
]

{ #category : #shout }
RubTextAreaExamples class >> hasBindingOf: aString [
	^ (self bindingOf: aString) notNil
	
]

{ #category : #shout }
RubTextAreaExamples class >> hasBindingThatBeginsWith: aString [
	^false
]

{ #category : #accessing }
RubTextAreaExamples class >> interactionModel [
	^ ModelForShout 
]

{ #category : #NOcompletion }
RubTextAreaExamples class >> isCodeCompletionAllowed [

	^ true.
]

{ #category : #accessing }
RubTextAreaExamples class >> menu [
	^ nil
]

{ #category : #examples }
RubTextAreaExamples class >> nicolaiAttributeFix [
	<example>
	"self nicolaiAttributeFix"
	| font1 font2 tMorph |
	font1 := TextFontReference toFont: (StrikeFont familyName: 'Bitmap DejaVu Sans' size: 11).
	font2 := TextFontReference toFont: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10).
	{font1.
	font2}
		do: [ :f | 
			| t1 t2 t3 t4 t5 t6 |
			tMorph := RubEditingArea new.
			t1 := 'normal\' withCRs asText addAttribute: f.
			t2 := 'bold\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis bold.
			t3 := 'underline\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis underlined.
			t4 := 'struck out\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis struckOut.
			t5 := 'italic\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis italic.
			t6 := 'narrow\' withCRs asText
				addAttribute: f;
				addAttribute: TextEmphasis narrow.
			tMorph updateTextWith: t1 , t2 , t3 , t4 , t5 , t6.
			tMorph openInWorld ]
]

{ #category : #examples }
RubTextAreaExamples class >> presentationInput [
	"
	
	self presentationInput.
	
	Thanks to Tudor Girba aka Doru
	Then Shift+Tab makes the editing area appear at the center of the screen.
	Useful to input explanations during a presentation 
	"

	KMRepository reset.
	KMRepository default
		setAsGlobalCategory:
			((KMCategory named: 'Presentation')
				addKeymapEntry:
					(KMKeymap
						shortcut: Character tab shift
						action: [ 
							RubEditingArea new
								beWrapped;
								width: World width / 3;
								updateTextWith: '';
								takeKeyboardFocus;
								backgroundColor: Color paleYellow;
								cellInset: 50;
								onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
								font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
								openCenteredInWorld ]))
]

{ #category : #examples }
RubTextAreaExamples class >> profileSourcesFileViewing [
	"The big test to check that a TextArea is able to compose and ''quickly'' show
	 a big file content "

	"self profileSourcesFileViewing"

	| contents |
	
	contents := nil.
	
	self
	show: 'Wait a moment loading text, the sources file is huge' translated
	while: [ 
		contents := (FileStream readOnlyFileNamed: Smalltalk sourcesFile basename) contents
	].

	TimeProfiler spyAllOn: [ 
		RubEditingArea new
			beWrapped;
			width: 600;
			updateTextWith: contents;
			openInWorld  
	]
]

{ #category : #examples }
RubTextAreaExamples class >> rightFlushed [
	"self rightFlushed"

	RubEditingArea new
		beWrapped;
		width: 300;
		updateTextWith: 'AAAAAA' , Character cr asString , 'BBBB';
		rightFlush;
		openInWorld
]

{ #category : #NOcompletion }
RubTextAreaExamples class >> selectedClassOrMetaClass [
	^ ModelForShout class
]

{ #category : #shout }
RubTextAreaExamples class >> shoutAboutToStyle: aStyler [
	aStyler classOrMetaClass: ModelForShout.
	^ true
]

{ #category : #examples }
RubTextAreaExamples class >> shoutedScriptingTextArea [
	"self shoutedScriptingTextArea" 

	ModelForShout := nil.
	RubEditingArea new
		beWrapped;
		model: self;
		beForSmalltalkScripting;
		width: 500;
		updateTextWith: self someSmalltalkScript;
		openInWorld
]

{ #category : #private }
RubTextAreaExamples class >> show: aMessage while: aBlock [
	| m |
	m := RubEditingArea new
		backgroundColor: Color black;
		beReadOnly;
		beWrapped;
		width: 300;
		setTextWith: aMessage;
		font: (LogicalFont familyName: 'Arial' pointSize: 20);
		textColor: Color white;
		openInWorld;
		centered;
		yourself.
	
	[ aBlock ensure: [ m delete ] ] fork.
]

{ #category : #examples }
RubTextAreaExamples class >> simplestEmptyNotWrapped [
	"self simplestEmptyNotWrapped"

	RubEditingArea new
		beNotWrapped;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> simplestEmptyTextField [
	"self simplestEmptyTextField"

	RubTextFieldArea new
		forbidMenu;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> simplestEmptyWrappedTo400 [
	"self simplestEmptyWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> simplestWrappedTo400 [
	"self simplestWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: self smallLoremIpsum;
		openInWorld
]

{ #category : #accessing }
RubTextAreaExamples class >> smallLoremIpsum [
	^'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. '
]

{ #category : #examples }
RubTextAreaExamples class >> smallLoremIpsumWrappedTo400 [
	"self smallLoremIpsumWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: self smallLoremIpsum;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> smalltalkCodeWithStabilo [
	"self smalltalkCodeWithStabilo"
	| area seg code |
	area := RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: (code := self someSmallSmalltalkCode).
	area addSegment: (seg := RubPlainSegmentMorph inTextArea: area from: 3 to: code size).
	seg color: (Color yellow).
	seg borderColor: (Color orange alpha: 0.5).
	seg beSmoothCurve.
	area openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> smalltalkCodeWrappedTo400 [
	"self smalltalkCodeWrappedTo400"
	| area |
	ModelForShout := self.
	area := RubEditingArea new
		beWrapped;
		model: self;
		width: 400;
		beForSmalltalkCodeWithDefaultFont;
		updateTextWith: self someSmalltalkCode.
	area openInWorld.
	^ area
]

{ #category : #examples }
RubTextAreaExamples class >> smalltalkCodeWrappedWithSegments [
	"self smalltalkCodeWrappedWithSegments"

	| area s |
	area := self smalltalkCodeWrappedTo400.
	area addSegment: (RubUnderlinedSegmentMorph inTextArea: area from: 1 to: 29).
	s := RubUnderlinedSegmentMorph inTextArea: area from: 43 to: 57.
	s straight: false.
	area addSegment: s.
	area addSegment: (s := RubUnderlinedSegmentMorph inTextArea: area from: 81 to: 185).
	s color: (Color blue alpha: 0.02).
	s borderColor: Color blue.
	s underlineColor: Color blue.
	area addSegment: (s := RubPlainSegmentMorph inTextArea: area from: 300 to: 500).
	s color: (Color yellow).
	s borderColor: (Color orange alpha: 0.5).
	s beSmoothCurve.
	s balloonColor: Color yellow.
	s setCenteredBalloonText: 'Please note this deferred execution'.
	area openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> someSmallSmalltalkCode [
	^ '
WorldState
	addDeferredUIMessage: [ 
		World displayWorldSafely.
		Cursor wait
			showWhile: [ 
				aBlock
					ensure: [ m delete ] ] ]'
]

{ #category : #accessing }
RubTextAreaExamples class >> someSmalltalkCode [
	^ self class sourceCodeAt: #show:while:
]

{ #category : #accessing }
RubTextAreaExamples class >> someSmalltalkScript [
	^ '" Enter some code here as in a workspace"' , String cr , 'RubTextArea new
		beWrapped;
		smalltalkScriptingModel: self;
		width: 500;
		setTextWith: ''" Enter some code here as in a workspace"'';
		openInWorld'
]

{ #category : #examples }
RubTextAreaExamples class >> sourceCodeTemplateWrappedTo400 [
	"self sourceCodeTemplateWrappedTo400"
	ModelForShout := self.
	RubEditingArea new
		model: self ;
		beForSmalltalkCode;
		beWrapped;
		width: 400;
		updateTextWith: self sourceCodeTemplate;
		openInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> textAreaWithBiggerFont [
	" self textAreaWithBiggerFont "

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: 'test';
		takeKeyboardFocus;
		onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
		font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
		openCenteredInWorld
]

{ #category : #examples }
RubTextAreaExamples class >> textFieldWithCRAction [
	"self textFieldWithCRAction"

	| s |
	s := RubTextFieldArea new
		width: 800;
		setTextWith: 'My contents will be printed in the Transcript when return is entered';
		forbidMenu;
		openInWorld.
	s announcer when: RubReturnEntered send: #value to:  [ Transcript show: s text asString ]
]

{ #category : #examples }
RubTextAreaExamples class >> withOnlyThePrimarySelection [
	"self withOnlyThePrimarySelection"

	ModelForShout := nil.
	RubEditingArea new
		beWrapped;
		width: 400;
		model: self;
		withoutAnyDecorator;
		updateTextWith: self smallLoremIpsum;
		openInWorld.
		

]

{ #category : #examples }
RubTextAreaExamples class >> wrappedTo400WithMarginsOf40 [
	"self wrappedTo400WithMarginsOf40"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateMarginsWith: 40 asMargin;
		updateTextWith: self smallLoremIpsum;
		openInWorld
]
