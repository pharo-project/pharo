actions
openHint: aHintString title: aTitleString type: aTypeSymbol for: aBrick
	|brick|
	
	self closeHint.
	brick :=  GLMPopupBrick new
		hintText: aHintString;
		titleText: aTitleString;
		perform: aTypeSymbol withEnoughArguments: {  };
		openFor: aBrick.
		
	hintBrick := brick
		
	