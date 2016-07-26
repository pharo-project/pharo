comparing
sonsOver: threshold
	"Returns all the sons that are not below a certain threshold. threshold is a number."
	| hereTally sons |
	(receivers isNil or: [ receivers isEmpty ]) ifTrue: [ ^#() ].
	hereTally := tally.
	sons := receivers select: [ :son | "subtract subNode tallies for primitive hits here"
		hereTally := hereTally - son tally.
		son tally > threshold ].
	hereTally > threshold
		ifTrue: [
			| last |
			last := self class new class: class method: method.
			last process: process.
			last reportOtherProcesses: reportOtherProcesses.
			^sons copyWith: (last primitives: hereTally)].
	^sons