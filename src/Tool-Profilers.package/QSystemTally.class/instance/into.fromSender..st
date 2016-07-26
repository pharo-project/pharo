report
into: leafDict fromSender: senderTally
	| leafNode |
	leafNode := leafDict at: method ifAbsentPut: [ QSystemTally new class: class method: method ].
	leafNode bump: tally fromSender: senderTally