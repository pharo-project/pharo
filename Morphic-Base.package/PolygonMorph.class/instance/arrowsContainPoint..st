geometry
arrowsContainPoint: aPoint
	"Answer an Array of two Booleans that indicate whether the given point is inside either arrow"

	| retval f |
	retval := { false . false }.
	(super containsPoint: aPoint) ifFalse: [^ retval ].
	(closed or: [arrows == #none or: [vertices size < 2]]) ifTrue: [^ retval].

	(arrows == #forward or: [arrows == #both]) ifTrue: [	"arrowForms first has end form"
		f := self arrowForms first.
		retval at: 2 put: ((f pixelValueAt: aPoint - f offset) > 0)
	].
	(arrows == #back or: [arrows == #both]) ifTrue: [ "arrowForms last has start form"
		f := self arrowForms last.
		retval at: 1 put: ((f pixelValueAt: aPoint - f offset) > 0)
	].
	^retval.