accessing
normal: aPoint
	"Change the effective normal of any oriented fill styles."

	|delta|
	aPoint ifNil: [
		self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs normal: nil]].
		^self].
	delta := aPoint - self normal.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs normal: fs normal + delta]]