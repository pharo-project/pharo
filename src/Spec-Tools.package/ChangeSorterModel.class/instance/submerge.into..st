change set
submerge: source into: destination
	"Copy the contents of the receiver to the other side, then remove the receiver -- all after checking that all is well."
	|   message |
	source == destination
		ifTrue: [ ^ false ].
	source isEmpty
		ifTrue: [ 
			self inform: 'Nothing to copy.  To remove,
simply choose "remove".'.
			^ false ].

	source okayToRemove ifFalse: [^ false].
	message := 'Please confirm:  copy all changes
in "', source name, '" into "', destination name, '"
and then destroy the change set
named "', source name, '"?'.
 
	(self confirm: message) ifFalse: [^ false].

	(source hasPreamble or: [source hasPostscript]) ifTrue:
		[(self confirm: 
'Caution!  This change set has a preamble or
a postscript or both.  If you submerge it into
the other side, these will be lost.
Do you really want to go ahead with this?') ifFalse: [^ false]].

	self copyAllChangesFrom: source to: destination.
	self removeChangeSet: source prompting: false.
	^ true