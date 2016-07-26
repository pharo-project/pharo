validation
defaultValidate: anObject on: aPort
	"we only allow objects that are in the display value"

	(aPort name == #selection) ifFalse: [ ^ true ].
	self entity isNil ifTrue: [ ^ false ].
	^ self isMultiple
		ifTrue: [ anObject isCollection and: [
				anObject allSatisfy: [:each | self displayValue includes: each ] ]]
		ifFalse: [ self displayValue includes: anObject ]