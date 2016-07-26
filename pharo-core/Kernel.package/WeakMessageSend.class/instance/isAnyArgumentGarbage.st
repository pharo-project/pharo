private
isAnyArgumentGarbage
	"Make sure that my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			(flag not and: [arg isNil])
				ifTrue: [^true]
		]
	].
	^false
