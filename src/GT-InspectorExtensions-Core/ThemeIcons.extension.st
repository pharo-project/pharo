Extension { #name : #ThemeIcons }

{ #category : #'*GT-InspectorExtensions-Core' }
ThemeIcons >> gtInspectorIconsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Icons';
		display: [ icons associations sorted: [ :a :b | a key < b key ] ];
		icon: [ :each | each value ];
		format: [ :each | each key ];
		filterOn: [ :pattern :each | '*' , pattern asString , '*' match: each key ];
		helpMessage: 'Filter icon names, e.g., user'
]
