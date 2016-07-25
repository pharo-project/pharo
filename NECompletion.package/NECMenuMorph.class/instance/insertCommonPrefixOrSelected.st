actions
insertCommonPrefixOrSelected
	"Return value: true if the user input has been handled, regardless of whether a prefix was inserted"

	| prefix |
	context model isEmpty ifTrue: [^ false].
	context model entries size = 1
		ifTrue: [ ^ self insertSelected ].
	prefix := context model commonPrefix.
	prefix = context model narrowString ifTrue:[^ self insertSelected].
	self flag: 'Pending issue 7308, "controller editor wordAtCaret" below should be changed to "context completionToken"'.
	prefix size > controller editor wordAtCaret size
		ifTrue: [ 
			self insertCompletion: prefix.
			context narrowWith: controller editor wordAtCaret ].
	^ true