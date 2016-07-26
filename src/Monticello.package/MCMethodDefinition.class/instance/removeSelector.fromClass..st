installing
removeSelector: aSelector fromClass: aClass
	"Safely remove the given selector from the target class.
	Be careful not to remove the selector when it has wandered
	to another package, but remove the category if it is empty."

	| newProtocol |
	newProtocol := aClass organization categoryOfElement: aSelector.
	newProtocol
		ifNotNil: [ 
			"If moved to and fro extension, ignore removal"
			(category beginsWith: '*') = (newProtocol beginsWith: '*')
				ifFalse: [ ^ self ].	"Check if moved between different extension categories"
			((category beginsWith: '*') and: [ category ~= newProtocol ])
				ifTrue: [ ^ self ] ].
	aClass removeSelector: aSelector.
	aClass organization removeProtocolIfEmpty: category