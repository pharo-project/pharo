rendering
defaultValidate: anObject on: aPort
	"we only allow objects that are in the display value"

	(aPort name == #selection) ifFalse: [ ^ true ].
	self entity isNil ifTrue: [ ^ false ].
	^ self displayValue includes: anObject 