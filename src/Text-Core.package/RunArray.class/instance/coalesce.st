adding
coalesce
	"Try to combine adjacent runs"
	| ind |
	ind := 2.
	[ind > values size] whileFalse: [
		(values at: ind-1) = (values at: ind) 
			ifFalse: [ind := ind + 1]
			ifTrue: ["two are the same, combine them"
				values := values copyReplaceFrom: ind to: ind with: #().
				runs at: ind-1 put: (runs at: ind-1) + (runs at: ind).
				runs := runs copyReplaceFrom: ind to: ind with: #().
				"self error: 'needed to combine runs' "]].
			