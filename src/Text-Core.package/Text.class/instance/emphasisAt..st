emphasis
emphasisAt: characterIndex
	"Answer the fontfor characters in the run beginning at characterIndex."
	| attributes |
	self size = 0 ifTrue: [^ 0].	"null text tolerates access"
	attributes := runs at: characterIndex.
	^attributes inject: 0 into:
		[:emph :att | emph bitOr: att emphasisCode].