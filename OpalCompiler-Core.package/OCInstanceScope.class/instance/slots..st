initializing
slots: slotCollection

	slotCollection do: [ :slot |
		vars at: slot name put:
			(OCSlotVariable new
				slot: slot;
				scope: self;
				yourself)].
