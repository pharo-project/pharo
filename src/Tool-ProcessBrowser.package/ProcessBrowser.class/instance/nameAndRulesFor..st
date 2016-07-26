process actions
nameAndRulesFor: aProcess 
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	aProcess == autoUpdateProcess ifTrue: [ ^{'my auto-update process'. true. true} ].
	^self class nameAndRulesFor: aProcess 
