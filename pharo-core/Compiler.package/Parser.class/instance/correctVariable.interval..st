error correction
correctVariable: proposedVariable interval: spot 
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| tempIvar binding userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: 
		[^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	"First check to see if the requestor knows anything about the variable"
	tempIvar := proposedVariable first isLowercase.
	(tempIvar and: [(binding := requestor bindingOf: proposedVariable) notNil]) 
		ifTrue: [^encoder global: binding name: proposedVariable].
	userSelection := requestor selectionInterval.
	requestor selectFrom: spot first to: spot last.
	"requestor select."

	"Build the menu with alternatives"
	action := UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	^action value