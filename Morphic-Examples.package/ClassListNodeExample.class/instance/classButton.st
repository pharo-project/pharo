menu
classButton
	^ ( self theme 
				newButtonIn: World
				for: self
				getState: nil
				action: #browseItem
				arguments: {}
				getEnabled: #enabled
				getLabel: nil
				help: 'Open a browser on ' translated , self item name)
				label: (self theme windowLabelForText: (self item name) , '...');
				yourself