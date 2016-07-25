accessing
label: aStringOrTextOrMorph font: aFont
	"Label this button with the given string or morph."
	| containerMorph |

	self removeAllMorphs.
	"nest label in a row for centering"
	containerMorph := AlignmentMorph new
		borderWidth: 0;
		layoutInset: 0;
		cellInset: 2;
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		wrapCentering: #center;
		listCentering: #center;
		cellPositioning: #center;
		yourself.
		
	(self iconPosition == #top or: [ self iconPosition == #bottom ]) 
		ifTrue: [ containerMorph setAsColumn ]
		ifFalse: [ containerMorph setAsRow ].

	label := aStringOrTextOrMorph.	
	self label ifNotNil: [ 
		labelMorph := self label isMorph
			ifTrue: [ self label ]
			ifFalse: [ self newLabel: aFont ].
		containerMorph addMorph: labelMorph ].

	self icon ifNotNil: [
		iconMorph := self icon asMorph.
		((self iconPosition == #right) or: [ self iconPosition == #bottom ])
			ifTrue: [ containerMorph addMorphBack: iconMorph ]
			ifFalse: [ containerMorph addMorphFront: iconMorph ] ].
	
	self addMorph: containerMorph.
	self updateLabelEnablement