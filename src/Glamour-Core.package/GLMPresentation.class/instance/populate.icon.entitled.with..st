scripting actions
populate: aPortSymbol icon: anIcon entitled: aString with: aBlock 
	 
	self 
		addAction: 
			((GLMGenericAction new) 
				action: 
						((GLMPortUpdater new) 
								portSymbol: aPortSymbol; 
								valueBlock: aBlock; 
								yourself); 
				icon: anIcon; 
				title: aString;
				yourself)