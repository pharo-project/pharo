argument validation
validate: anObject shouldBe: aClass
	"I allow for nil values."
	(anObject isNil or: [ anObject isKindOf: aClass ])
		ifFalse: [ LGitIllegalArgument signalWith: anObject shouldBe: aClass ]