querying
name

	name == nil ifTrue: [ name := self extractNameFromFile ].
	^name