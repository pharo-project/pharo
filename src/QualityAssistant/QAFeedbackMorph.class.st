"
I provide a small morph for providing feedback about critics from Nautilus plugin
"
Class {
	#name : #QAFeedbackMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'positive',
		'negative',
		'text',
		'critique'
	],
	#category : 'QualityAssistant'
}

{ #category : #accessing }
QAFeedbackMorph >> critique [
	^ critique
]

{ #category : #accessing }
QAFeedbackMorph >> critique: anObject [
	critique := anObject
]

{ #category : #initialization }
QAFeedbackMorph >> finalizeFeedback: aBoolean withComment: aString [

	text contents: 'Thanks!'.
	self
		removeMorph: positive;
		removeMorph: negative.
	
	[ aBoolean
		ifTrue:  [ critique providePositiveFeedback: aString ]
		ifFalse: [ critique provideNegativeFeedback: aString ]
	] fork.

	[ 2 seconds asDelay wait.
	  self delete ] fork.
]

{ #category : #initialization }
QAFeedbackMorph >> fireDialogWithFeedback: aBoolean [

	| comment |

	^ ((GLMFormBrick new
		color: Smalltalk ui theme textColor negated;
		width: 400;
		vShrinkWrap;
		label: [ :header | header
			text: (aBoolean
				ifTrue:  [ 'Positive' ]
				ifFalse: [ 'Negative' ]), ' Feedback';
			hSpaceFill;
			fontSize: 16 ];
		text: [ :criticText | criticText
			text: 'about critique: ', (critique description surroundedBy: '"');
			hSpaceFill;
			margin: 0 ];
		label: [ :commentText | commentText
			hSpaceFill;
			text: 'Comment (optional):';
			marginTop: 10 ];
		addBrickBack: (
			comment := GLMRubScrolledTextBrick new
				borderWidth: 1;
				borderColor: Smalltalk ui theme textColor;
				hSpaceFill;
				height: 150;
				yourself);
		
		with: [ :form | form 
			addBrickBack: (GLMBrick new
			useHorizontalLinearLayout;
			hShrinkWrap;
			vShrinkWrap;
			hAlign: #right;
			addBrickBack: (form newButton
				marginRight: 3;
				text: 'Send';
				when: #onClicked do: [
					form window delete.
					self finalizeFeedback: aBoolean withComment: comment text asString ]);
			addBrickBack: (form newButton
				marginLeft: 3;
				text: 'Cancel';
				when: #onClicked do: [ form window delete ]);
			yourself)
		];
	asBrickedMorph)
		openInWindowLabeled: '') beUnresizeable in: [ :window |
			self window modalLockTo: window ]
]

{ #category : #initialization }
QAFeedbackMorph >> initialize [
	super initialize.
	
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellInset: 3;
		layoutInset: 3@0;
		cornerStyle: #rounded;
		color: Color transparent;
		borderColor: (Color black alpha: 0.1).
		
	self
		addMorphBack: (text := 'Helpful?' asMorph);
		addMorphBack: (self initializePositiveFeedback);
		addMorphBack: (self initializeNegativeFeedback)
]

{ #category : #initialization }
QAFeedbackMorph >> initializeNegativeFeedback [
	
	^ negative := IconicButton new
				target: self;
				labelGraphic: (self iconNamed: #thumbsDownIcon);
				actionSelector: #fireDialogWithFeedback:;
				arguments: { false };
				color: Color transparent;
				helpText: 'This critic is not helpful';
				extent: 16 @ 16;
				borderWidth: 0;
				yourself
]

{ #category : #initialization }
QAFeedbackMorph >> initializePositiveFeedback [
	
	^ positive := IconicButton new
				target: self;
				labelGraphic: (self iconNamed: #thumbsUpIcon);
				actionSelector: #fireDialogWithFeedback:;
				arguments: { true };
				color: Color transparent;
				helpText: 'This critic is helpful';
				extent: 16 @ 16;
				borderWidth: 0;
				yourself
]
