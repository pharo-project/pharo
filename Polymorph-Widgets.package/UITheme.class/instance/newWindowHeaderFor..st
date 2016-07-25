morph creation
newWindowHeaderFor: aWindow

	^ (AlignmentMorph newSpacer: Color transparent)
			vResizing: #spaceFill;
			layoutPolicy: ProportionalLayout new