initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'PopUpMenu confirm: `@object1 orCancel: `@object2'
			'PopUpMenu confirm: `@object1 trueChoice: `@object2 falseChoice: `@object3'
			'PopUpMenu confirm: `@object1'
			'PopUpMenu inform: `@object1'
			'PopUpMenu initialize'
			'PopUpMenu labelArray: `@object1 lines: `@object2'
			'PopUpMenu labelArray: `@object1'
			'PopUpMenu labels: `@object1 lines: `@object2'
			'PopUpMenu labels: `@object1'
			'PopUpMenu withCaption: `@object1 chooseFrom: `@object2'
			 )
		do: [ :node :answer | node ]