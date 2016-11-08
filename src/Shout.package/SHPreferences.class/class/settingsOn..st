settings
settingsOn: aBuilder 
	<systemsettings>
	(aBuilder setting: #'Syntax Highlighting') 
		target: self;
		dialog: [ self styleTableRow ];
		parentName: #codeBrowsing;
		description: 'Syntax Highlighting As You Type: Enable syntax highlighting in browsers, debuggers and workspaces and set patterns style.';
		selector: #enabled;
		iconName: #smallConfigurationIcon;
		with: [
			(aBuilder setting: #selectorPatternsStyle) 
				label: 'Selector Patterns';
				description: 'Selector patterns in method pane'.
			(aBuilder setting: #errorStyle) 
				label: 'Syntactic error';
				description: 'Invalid and undefined code' .
			(aBuilder setting: #argsStyle) 
				label: 'Parameters';
				description: 'Parameters in patterns, message sends, and blocks' .
			(aBuilder setting: #commentStyle) 
				label: 'Comments';
				description: 'Comments in code pane' .
			(aBuilder setting: #defaultStyle) 
				label: 'Default' ;
				description: 'Default style' .
			(aBuilder setting: #globalVarStyle) 
				label: 'Global variables' ;
				description: 'References to global variables, including classes' .
			(aBuilder setting: #instanceVarStyle) 
				label: 'Instance/class variables' ;
				description: 'References to instance and class variables' .
			(aBuilder setting: #messageSendsStyle) 
				label: 'Message sends' ;
				description: 'Message sends' .
			(aBuilder setting: #primitiveOrModuleStyle) 
				label: 'Primitive or module' ;
				description: 'Primitive or module' .
			(aBuilder setting: #primitiveTypesStyle) 
				label: 'Primitive types';
				description: 'Literal data' .
			(aBuilder setting: #reservedWordsStyle) 
				label: 'Reserved words';
				description: 'Reserved words of the Smalltalk language' .
			(aBuilder setting: #syntaxStyle) 
				label: 'Syntax';
				description: 'Any other syntactic element' .
			(aBuilder setting: #variableStyle) 
				label: 'Variable';
				description: 'Temporary variable'  ]