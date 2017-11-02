Extension { #name : #Object }

{ #category : #'*GT-SpotterExtensions-Core' }
Object >> gtInspectorActionSpot [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ 
			GTSpotterMorph new
				extent: (World width / 2.4 @ (World height / 1.6)) asIntegerPoint;
				"extent: 400@500;"
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
]
