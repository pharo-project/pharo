private
ensureArguments
	"Return true if my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			arg ifNil: [ flag ifFalse: [ ^false ]]
		]
	].
	^true
