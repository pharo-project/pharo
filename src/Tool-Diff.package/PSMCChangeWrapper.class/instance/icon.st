accessing
icon
	"Answer a form with an icon to represent the receiver"

	| o |
	o := self operation.
	o isNil
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeEmptyIcon ].
	o isAddition
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeAddIcon ].
	o isRemoval
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeRemoveIcon ].
	^ Smalltalk ui icons iconNamed: #changeUpdateIcon