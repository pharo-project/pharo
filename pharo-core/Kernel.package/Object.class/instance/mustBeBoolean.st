block support
mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the VM.  The sending context is rewound to just before the jump causing this exception."

	^ self mustBeBooleanInMagic: thisContext sender