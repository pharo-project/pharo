actions
actionRemove: anAction
	(UIManager default confirm: ('Exclude {1} from this release/commit?' format: { anAction displayString })) ifFalse: [ ^ self ].
	
	anAction removeOn: self unit.
	"Do not refresh after removing: refresh will force a 
	 recalculation of commit unit"