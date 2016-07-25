process control
nameAndRulesFor: aProcess 
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	| rules |
	rules := {nil. true. true}.
	WellKnownProcesses do: [:blockAndRules |
		blockAndRules key value == aProcess 
			ifTrue: [ rules := blockAndRules value value ]].
	rules first ifNil: [ 
		rules at: 1 put: aProcess suspendedContext asString ].
	^ rules