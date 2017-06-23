acessing
status
	"entity could be a repository or a package"
	[
		| status | 
		self repository isMissing ifTrue: [ ^ 'Local repository missing' asText allBold makeAllColor: self colorError ].
		self isLoaded ifFalse: [ ^ 'Not loaded' ].

		status := OrderedCollection new.
		entity isModified ifTrue: [ status add: 'Uncommited changes' ].
		
		^ status 
			ifNotEmpty: [ status asCommaString ]
			ifEmpty: [ 'Up to date' ] ] 
	on: Error do: [ :error | ^ error description asText makeAllColor: self colorError ]

