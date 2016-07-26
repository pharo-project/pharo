helpers-building
makeWithLayout: aClassLayout andSlots: someSlots
	"
	I create a class using the Slot class builder given a class layout and a collection of slots.
	"

	^self make: [:builder|
		builder
			layoutClass: aClassLayout;
			slots: someSlots
		].