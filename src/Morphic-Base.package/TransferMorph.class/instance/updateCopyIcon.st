private
updateCopyIcon

	| copyIcon |	
	copyIcon := self submorphWithProperty: #tmCopyIcon.
	self shouldCopy ifFalse: [
		^ copyIcon ifNotNil: [ copyIcon delete ]].
	copyIcon ifNotNil: [ ^ self ].
	copyIcon := ImageMorph new form: CopyPlusIcon.
	copyIcon setProperty: #tmCopyIcon toValue: true.
	^ self addMorphFront: copyIcon.
