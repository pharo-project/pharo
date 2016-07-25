*GT-InspectorExtensions-Core
gtInspectorSubmorphsIn: composite
	<gtInspectorPresentationOrder: 80>  
	composite tree 
		title: 'Submorphs';
		rootsExpanded;
		display: [:each | {each} ];
		icon: [:each | | morphForm |
			morphForm := each imageForm.
			(morphForm extent x > 0 and: [morphForm extent y > 0 ])
				ifTrue: [(morphForm scaledToSize: 16@16) asMorph]
				ifFalse: [Form extent: 16@16] ];
		children: [:each | each submorphs];
		when: [:each | each submorphs notNil and:[ each hasSubmorphs ] ]