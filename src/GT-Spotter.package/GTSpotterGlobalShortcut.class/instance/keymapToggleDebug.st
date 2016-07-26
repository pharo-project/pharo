shortcuts
keymapToggleDebug
	"KMRepository reset"
	"This shortcut is still using command because the current Virtual Machine interprets Ctrl-cr as a different KEY.
	This should be replaced for #meta as soon as the new VMs are fixed and shipped"
	<shortcut>
	^ KMKeymap
			shortcut: Character cr command shift
			action: [ self class openGlobalSpotterDebug ]