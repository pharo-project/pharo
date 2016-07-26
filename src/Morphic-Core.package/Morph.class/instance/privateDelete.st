submorphs-add/remove
privateDelete
	"Remove the receiver as a submorph of its owner"
	owner ifNotNil:[owner removeMorph: self].