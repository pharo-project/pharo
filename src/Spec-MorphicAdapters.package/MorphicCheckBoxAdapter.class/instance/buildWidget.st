factory
buildWidget

	| checkButton |
	checkButton := CheckboxMorph
						on: self 
						selected: #state
						changeSelected: #state:. 			
	checkButton 
			label:	self label; 
			beCheckbox;
			hResizing: #spaceFill; 
			vResizing: #shrinkWrap; 
			setBalloonText: self help;
			getEnabledSelector: #enabled;
			dragEnabled: self dragEnabled;
			dropEnabled: self dropEnabled;	
			labelClickable: self labelClickable.
	^ checkButton