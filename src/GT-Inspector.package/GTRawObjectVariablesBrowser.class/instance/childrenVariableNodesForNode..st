private building
childrenVariableNodesForNode: aNode
	| variableNodes |

	"Nil should have now a single child: `self`. For now this is needed for
	a smooth navigation in the tree widget."
	"anObject ifNil: [ ^ #() ]."

	variableNodes := OrderedCollection new: aNode value class allSlots size.
	variableNodes add: (GTInspectorWrapperNode hostObject: aNode label: 'self').
	self basicVariableNodesFor: aNode value in: variableNodes.
	^ variableNodes