accessing
icon
	"Answer a form with an icon to represent the receiver"

	| o |
	o := self operation.
	o isNil
		ifTrue: [ ^ self iconNamed: #changeEmptyIcon ].
	o isAddition
		ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	o isRemoval
		ifTrue: [ ^ self iconNamed: #changeRemoveIcon ].
	^ self iconNamed: #changeUpdateIcon