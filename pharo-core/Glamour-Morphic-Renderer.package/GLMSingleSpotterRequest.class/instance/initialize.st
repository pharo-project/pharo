initialization
initialize
	super initialize.
	prompt := 'Completion Request'.
	default := String new.
	assisted := false.
	searchBlock := [ :value | #() ].
	labelBlock := [ :value | value asString ].
	iconBlock := [ :value | nil ]