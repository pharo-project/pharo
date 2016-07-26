class building
layoutChanged: aClass
	super layoutChanged: aClass.
	"we check on the slot level as the slot is hidden"
	(aClass classLayout hasSlotNamed: baseSlot name) ifTrue: [
		baseSlot := aClass classLayout slotNamed: baseSlot name. "make sure we all use the same baseSlot"
		^self. ].
	"here we need to be more clever: if a subclass defines it, 
	move the difiniton here"
	self flag: #TBD.
	aClass addSlot: (baseSlot := #'_propertyBaseSlot' => BaseSlot default: Dictionary new).
	