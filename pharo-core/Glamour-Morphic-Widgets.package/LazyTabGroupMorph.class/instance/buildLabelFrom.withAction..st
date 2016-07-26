private
buildLabelFrom: aStringOrMorph withAction: aBrick
	| label innerLabel |

	innerLabel := aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [
			|container labelMorph|
			container := GLMBrick new.
			labelMorph := GLMLabelBrick new
				text: aStringOrMorph;
				textColor: container themer textSecondaryColor;
			 	hSpaceFill;
			 	vSpaceFill;
				marginLeft: 5.
			container
				addBrickBack: labelMorph;
				hSpaceFill;
				vSpaceFill.
			
			container maxWidth: [ 5 + labelMorph approximateWidth + (aBrick ifNotNil: [ :b | b normalImage width * 1.5 ] ifNil: [ 0 ])].
			container ].
		
	label := aBrick
			ifNil: [ innerLabel ]
			ifNotNil: [ aBrick vSpaceFill; width: (aBrick normalImage width * 1.5). innerLabel addBrickBack: aBrick. innerLabel ].
					
	^ label