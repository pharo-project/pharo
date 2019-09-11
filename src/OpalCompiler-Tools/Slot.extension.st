Extension { #name : #Slot }

{ #category : #'*OpalCompiler-Tools' }
Slot >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite list 
		title: 'Users';
		display: [ self usingMethods collect: #asRingDefinition];
		when: [ self isReferenced ];
		showOnly: 30
]
