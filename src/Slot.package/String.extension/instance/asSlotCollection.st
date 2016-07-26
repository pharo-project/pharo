*Slot
asSlotCollection
	"Parse as slots.
	Example: 
	  'a b' asSlotCollection --> {a => Slot. b => Slot}
	"
	^self substrings collect: [ :substring | substring asValidInstVarName asSlot ]