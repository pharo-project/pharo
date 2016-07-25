menu - class
copyClassFrom: src to: dest
	"Place these changes in the other changeSet also"

	| otherChangeSet |
	self checkThatSidesDiffer: [ ^ self ].
	self okToChange ifFalse: [ ^ self inform: 'Can''t discard edits.' ].
	src selectedClass ifNil: [ ^ self inform: 'Selected class is nil.' ].
	otherChangeSet := dest selectedChangeSet.
	
	self model copyClass: src selectedClass from: src selectedChangeSet to: otherChangeSet.
	
	dest setSelectedChangeSet: otherChangeSet.