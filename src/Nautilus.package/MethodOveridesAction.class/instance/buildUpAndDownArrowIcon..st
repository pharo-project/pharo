private
buildUpAndDownArrowIcon: aMethod

	| container up down |
	container := Morph new.
	container 
		extent: 12@12;
		color: Color transparent.
	up := IconicButton new 
				target: self browser;
				actionSelector: #arrowUp:;
				arguments: { aMethod };
				labelGraphic: (self iconNamed: #arrowDoubleUpIcon);
				color: Color transparent;
				extent: 16 @ 8;
				helpText: 'Browse overriden message';
				borderWidth: 0.
	down :=  IconicButton new 
				target: self browser;
				actionSelector: #arrowDown:;
				arguments: { aMethod };
				labelGraphic: (self iconNamed: #arrowDoubleDownIcon);
				color: Color transparent;
				extent: 16 @ 8;
				helpText: 'Browse overriding messages';
				borderWidth: 0.
	^ (container
		changeTableLayout;
		listDirection: #topToBottom;
		addMorph: down;
		addMorph: up;
		yourself) -> {up. down}.