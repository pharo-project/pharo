event handling - private
transform: anEvent from: originalEvent andSendTo: aMorph

	| transformedEvent |

	transformedEvent := anEvent transformedBy: (aMorph transformedFrom: originalEvent hand).
	^ aMorph handleEvent: transformedEvent