order
privateActionIcon
	"Return the icon for this action"

	isOverride
		ifTrue: [ 
			isOverridden
				ifTrue: [
					result := self buildUpAndDownArrowIcon: method.
					^ result key ]
				ifFalse: [ 
					^ IconicButton new
						target: self browser;
						actionSelector: #arrowUp:;
						arguments: {method};
						labelGraphic: (self iconNamed: #arrowUpIcon);
						color: Color transparent;
						helpText: 'Browse overriden message';
						extent: self iconExtent;
						borderWidth: 0 ] ]
		ifFalse: [ 
			isOverridden
				ifTrue: [ 
					^ IconicButton new
						target: self browser;
						actionSelector: #arrowDown:;
						arguments: {method};
						labelGraphic: (self iconNamed: #arrowDownIcon);
						color: Color transparent;
						helpText: 'Browse overriding messages';
						extent: self iconExtent;
						borderWidth: 0 ] ]