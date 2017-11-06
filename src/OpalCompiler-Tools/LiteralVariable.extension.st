Extension { #name : #LiteralVariable }

{ #category : #'*OpalCompiler-Tools' }
LiteralVariable >> gtInspectorUsersIn: composite [
	<gtInspectorPresentationOrder: 30>
	| users |
	users := self usingMethods.
	composite list 
		title: 'Users';
		display: [ users];
		when: [ users notEmpty ];
		showOnly: 30
]
