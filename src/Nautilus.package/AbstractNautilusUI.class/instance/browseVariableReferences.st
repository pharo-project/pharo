buttons behavior
browseVariableReferences

	| mainMenu classVarSubMenu |

	self selectedClass ifNil: [ ^self ].
	
	classVarSubMenu := MenuMorph new.
	
	self 
	buildVariableMenuFor: classVarSubMenu 
	variablesFrom: #classVarNames 
	menuItemBy: [ :variable :variableClass :menu |
		
		menu 			 
		add: variable asString
		target: [ 
			self systemNavigation browseAllReferencesTo: (variableClass classPool associationAt: variable) 
		]
		selector: #value.
		
		menu lastItem icon: (self iconNamed: #classIcon)
		
	].
	
	mainMenu := MenuMorph new.
	mainMenu 
	buildTitle: [ :menuTitle | menuTitle
		title: (self selectedClass printString);
		color: Color transparent;
		font: StandardFonts windowTitleFont
	];
	add: 'Class Variables' subMenu: (classVarSubMenu).
	
	self 
	buildVariableMenuFor: mainMenu 
	variablesFrom: #instVarNames 
	menuItemBy: [ :variable :variableClass :menu |
		
		menu 			 
		add: variable asString
		target: [ 
			self systemNavigation browseAllAccessesTo: variable from: self selectedClass
		]
		selector: #value.
		
	].
	
	mainMenu popUpInWorld.
