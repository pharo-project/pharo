Extension { #name : #MCRepository }

{ #category : #'*GT-InspectorExtensions-Core' }
MCRepository >> gtInspectorGoferReferencesIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Gofer';
		display: [ self goferReferences sorted ];
		format: [ :each | each name asString ];
		icon: [ Smalltalk ui icons iconNamed: #packageIcon ];
		act: [ :com | 
			com selection
				ifNotNil: [ :each | 
					Gofer new
						repository: com entity;
						version: each name;
						load ] ]
			iconName: #downIcon
			entitled: 'Load'
]
