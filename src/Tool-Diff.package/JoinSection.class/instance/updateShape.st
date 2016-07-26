as yet unclassified
updateShape
	"Update the receiver's shape."

	(self src range isNil or: [self dst range isNil]) ifTrue: [^self].
	self shape: (Polygon vertices:
		{(0@ self src range first) + self src offset. (self width @ self dst range first) + self dst offset.
		(self width @ self dst range last) + self dst offset. (0@self src range last) + self src offset})