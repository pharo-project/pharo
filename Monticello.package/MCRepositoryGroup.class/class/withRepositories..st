instance creation
withRepositories: aCollection 
	| group |
	group  := self new.
	aCollection do: [ :each | group addRepository: each ].
	^ group 
		