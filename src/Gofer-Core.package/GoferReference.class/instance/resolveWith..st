querying
resolveWith: aGofer
	"Answer a single resolved reference with aGofer configuration, throw an error if the version can't be found.'"

	| references |
	references := self resolveAllWith: aGofer.
	^ references isEmpty
		ifTrue: [ self error: 'Unable to resolve ' , self name ]
		ifFalse: [ references last ]