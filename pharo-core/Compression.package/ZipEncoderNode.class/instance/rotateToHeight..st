encoding
rotateToHeight: maxHeight
	"Rotate the tree to achieve maxHeight depth"
	| newParent |
	height < 4 ifTrue:[^self].
	self left: (left rotateToHeight: maxHeight-1).
	self right: (right rotateToHeight: maxHeight-1).
	height := (left height max: right height) + 1.
	height <= maxHeight ifTrue:[^self].
	(left height - right height) abs <= 2 ifTrue:[^self].
	left height < right height ifTrue:[
		right right height >= right left height ifTrue:[
			newParent := right.
			self right: newParent left.
			newParent left: self.
		] ifFalse:[
			newParent := right left.
			right left: newParent right.
			newParent right: right.
			self right: newParent left.
			newParent left: self.
		].
	] ifFalse:[
		left left height >= left right height ifTrue:[
			newParent := left.
			self left: newParent right.
			newParent right: self.
		] ifFalse:[
			newParent := left right.
			left right: newParent left.
			newParent left: left.
			self left: newParent right.
			newParent right: self.
		].
	].
	parent computeHeight.
	^parent