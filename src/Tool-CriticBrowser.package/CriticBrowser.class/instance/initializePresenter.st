initialization
initializePresenter

	rulesModel whenSelectedItemChanged: [ :rule |
		(rule isNil or: [ rule isComposite ]) ifFalse: [ 
				criticModel 
					resetSelection;
	 				rule: rule;	
					setTextModelForNil
			].
				
			self setTitle: ( 
				rule 
					ifNil: [ self defaultTitle  ] 
					ifNotNil: [ rule name ]
			)
			
	].

