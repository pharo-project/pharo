private
executeNotifying: aBlock 
	| undos undo |
	undos := changes collect: [ :each | each executeNotifying: aBlock ].
	undo := self copy.
	undo changes: undos reversed.
	^ undo