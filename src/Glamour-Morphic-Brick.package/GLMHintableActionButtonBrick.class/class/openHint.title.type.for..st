actions
openHint: aHintString title: aTitleString type: aTypeSymbol for: aBrick
	|brick|
	
	self closeHint.
	brick :=  GLMPopupBrick new
		beUpward;
		hintText: aHintString;
		titleText: aTitleString;
		perform: aTypeSymbol withEnoughArguments: {  };
		openOn: aBrick.
		
	hintBrick := brick
		
	