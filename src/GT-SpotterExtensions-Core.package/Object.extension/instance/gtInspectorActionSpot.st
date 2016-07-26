*GT-SpotterExtensions-Core
gtInspectorActionSpot
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ 
			GTSpotterMorph new
				extent: 400@500;
				doLayout;
				spotterModel: (GTSpotter on: self);
				openCenteredInWorld
			 ];
		icon: GLMUIThemeExtraIcons glamorousSearch;
		condition: [ 
				| fakeStep |
				fakeStep := GTSpotter new newStep origin: self.
				self spotterProcessorsFor: fakeStep.
				fakeStep processors notEmpty ];
		title: 'Search'