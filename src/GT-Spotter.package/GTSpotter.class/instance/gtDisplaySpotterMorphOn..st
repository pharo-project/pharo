printing
gtDisplaySpotterMorphOn: aComposite
	<gtInspectorPresentationOrder: 50>
	
	aComposite morph 
		title: 'Live';
		display: [ 
			GLMBrick new 
				hSpaceFill;
				vSpaceFill;
				addMorphBack: 
					(GTSpotterMorph new
						spotterModel: self);
				yourself ]