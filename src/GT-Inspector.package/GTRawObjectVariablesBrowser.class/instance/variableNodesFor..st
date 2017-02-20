private building
variableNodesFor: anObject
	| variableNodes |

	"Nil should have now a single child: `self`. For now this is needed for
	a smooth navigation in the tree widget."
	"anObject ifNil: [ ^ #() ]."

	variableNodes := OrderedCollection new: anObject class allSlots size.
	variableNodes add: (GTInspectorSelfNode hostObject: anObject).
	self basicVariableNodesFor: anObject in: variableNodes.
	^ variableNodes