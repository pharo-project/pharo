initialization
fireDialogWithFeedback: aBoolean

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