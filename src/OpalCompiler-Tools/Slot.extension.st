Extension { #name : #Slot }

{ #category : #'*OpalCompiler-Tools' }
Slot >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	| users |
	users := self usingMethods.
	composite list 
		title: 'Users';
		display: [ users collect: #asRingDefinition];
		when: [ users notEmpty ];
		showOnly: 30
]
