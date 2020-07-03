Extension { #name : #Variable }

{ #category : #'*OpalCompiler-Tools' }
Variable >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	
	composite list 
		title: 'Users';
		display: [ self usingMethods collect: [:each | each asRingDefinition]];
		when: [ self isReferenced ];
		showOnly: 30
]
