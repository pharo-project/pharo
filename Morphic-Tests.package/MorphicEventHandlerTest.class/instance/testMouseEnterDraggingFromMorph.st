tests-events
testMouseEnterDraggingFromMorph

	| event | 
	event := MouseEvent basicNew setType: #mouseOver position: nil buttons: 2 hand: Morph new.
	morph eventHandler on: #mouseEnterDragging send: #value to: true.
	
	self assert: ((morph handleMouseEnter: event) == true)