accessing
openEditor
	| rules |
	rules := self failedRules.
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]