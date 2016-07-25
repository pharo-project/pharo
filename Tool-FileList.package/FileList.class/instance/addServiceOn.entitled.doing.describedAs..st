volume menu
addServiceOn: provider entitled: title doing: aSelector describedAs: description

	self addService: ((SimpleServiceEntry 
							provider: provider 
							label: title
							selector: aSelector
							description: description)
							usingLineAfter: false;
							yourself)