initialization
initializeWidgets
	
	title := self defaultTitle.
	rulesModel := self newTree.
	resetButton := self newButton.
	logButton := self newButton.
	
	self setLogButton.
	self setResetButton.
	 
	criticModel := SingleCodeCriticResultList new.
	rulesModel childrenBlock: [ :rule | 
		rule isComposite 
			ifTrue: [ rule rules ]
			ifFalse: [ #() ]]. 

	rulesModel displayBlock: [ :rule | self stringMorphForRule: rule ].
	
	rulesModel menu: [ :aMenu :shifted | self menu: aMenu shifted: shifted ].	
	
	self focusOrder
		add: rulesModel;
		add: criticModel