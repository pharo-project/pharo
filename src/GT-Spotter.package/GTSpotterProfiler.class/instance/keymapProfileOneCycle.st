shortcuts
keymapProfileOneCycle
	"KMRepository reset"
	<shortcut>
	^ KMKeymap
			shortcut: $u meta
			action: [ self class profileOneCycle ]