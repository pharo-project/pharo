private
leadingTab
	"return true if only tabs lie to the left"
	line first to: lastIndex do:
		[:i | (text at: i) == Tab ifFalse: [^ false]].
	^ true