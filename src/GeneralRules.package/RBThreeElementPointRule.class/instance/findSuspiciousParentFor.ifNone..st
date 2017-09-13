running
findSuspiciousParentFor: aNode ifNone: alternativeBlock
	| tentativeNode |

	tentativeNode := aNode parent.
	tentativeNode ifNil: [ ^ alternativeBlock value ].
	tentativeNode isMessage ifFalse: [
		^ self
			findSuspiciousParentFor: tentativeNode
			ifNone: alternativeBlock ].
	tentativeNode isBinary ifFalse: [ ^ alternativeBlock value ].
	tentativeNode selector = #@ ifTrue: [ ^ tentativeNode ].
	
	^ self
		findSuspiciousParentFor: tentativeNode
		ifNone: alternativeBlock