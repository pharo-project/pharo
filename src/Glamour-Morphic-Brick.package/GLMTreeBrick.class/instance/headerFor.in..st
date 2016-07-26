as yet unclassified
headerFor: anObject in: aCollapsable
	|header|
	
	header := GLMBrick new.
	header
		hSpaceFill;
		vShrinkWrap.
		
	header addBrickBack:
		((anObject isVariableBinding
			ifTrue: [ anObject key asBrick ]
			ifFalse: [ anObject asBrick ])
				hSpaceFill).
				
	(self hasChildren: anObject) ifTrue: [
		header addBrickFirst: (self newToggleButtonFor: aCollapsable) ].
	
	^ header