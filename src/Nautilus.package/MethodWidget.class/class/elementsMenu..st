menu
elementsMenu: aBuilder
	<nautilusGlobalMethodMenu>
	| selectedMethods tmp target |
	target := aBuilder model.
	selectedMethods := target selectedMethods.
	
	(aBuilder item: #'ToggleBreakPoint')
		action: [ target toggleBreakpoint ];
		order: 150;
		enabledBlock: [ target selectedMethod notNil ];
		label: ((target selectedMethod isNil or: [ target selectedMethod hasBreakpoint not ])
				ifTrue: [ 'Add breakpoint']
				ifFalse: [ 'Remove breakpoint'] ).
	
	(aBuilder item: #'ToggleBreakPointOnce')
		action: [ target toggleBreakpointOnce ];
		order: 151;
		enabledBlock: [ target selectedMethod notNil and: [ target selectedMethod hasBreakpoint not ] ];
		label: 'Add break once'.
		
	(aBuilder item: #'ToggleBreakPointCondition')
		action: [ target toggleBreakpointCondition ];
		order: 152;
		enabledBlock: [ target selectedMethod notNil and: [ target selectedMethod hasBreakpoint not ] ];
		label: 'Add break condition...';
		withSeparatorAfter.	
		

	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [ target findMethod ];
		order: 199;
		help: 'Search for a method by name'.
		
	tmp := ((aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target fullBrowse ];
		order: 200;
		yourself).
		
	target selectedMethod ifNil: [
		tmp withSeparatorAfter.
		 ^ target ].
	
	(aBuilder item: #'Generate test and jump')
		keyText: 'h, j' if: Nautilus useOldStyleKeys not;
		keyText: 'j' if: Nautilus useOldStyleKeys;
		action: [ target generateTestMethodsAndFocus: true ];
		order: 300.
		
	(aBuilder item: #'Generate test')
		keyText: 'h, J' if: Nautilus useOldStyleKeys not;
		keyText: 'J' if: Nautilus useOldStyleKeys;
		action: [ target generateTestMethodsAndFocus: false ];
		order: 400;
		withSeparatorAfter.
		
	(target selectedMethods allSatisfy: [:meth | meth isTestMethod ])
		ifTrue: [
			(aBuilder item: #'Run tests')
				keyText: 'j, m' if: Nautilus useOldStyleKeys not;
				keyText: 't' if: Nautilus useOldStyleKeys;
				action: [ target runTestForMethods: selectedMethods notifying: true ];
				order: 1000. ].
	
	target selectedMethod correspondingTestMethod notNil
		ifTrue: [
			(aBuilder item: #'Debug test')
				keyText: 'd' if: Nautilus useOldStyleKeys not;
				action: [ target debugTest ];
				order: 1001 ].		
			
	(aBuilder item: #'Senders of...')
		keyText: 'b, n' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [ target browseSendersOfMessages ];
		order: 1100;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Implementors of...')
		keyText: 'b, m' if: Nautilus useOldStyleKeys not;
		keyText: 'm' if: Nautilus useOldStyleKeys;
		action: [ target browseMessages ];
		order: 1200;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Inheritance')
		keyText: 'b, i' if: Nautilus useOldStyleKeys not;
		keyText: 'i' if: Nautilus useOldStyleKeys;
		action: [ target methodHierarchy ];
		order: 1300;
		enabledBlock: [ target enableMethodSingleSelection ].
		
	(aBuilder item: #'Versions')
		keyText: 'm, v' if: Nautilus useOldStyleKeys not;
		keyText: 'v' if: Nautilus useOldStyleKeys;
		action: [ target browseVersions ];
		order: 1400;
		enabledBlock: [ target enableMethodSingleSelection ];
		withSeparatorAfter.
	
	(aBuilder item: #'Categorize method')
		keyText: 'm, m' if: Nautilus useOldStyleKeys not;
		action: [ target categorizeMethod ];
		order: 1500.
		
	(aBuilder item: #'Move to package...')
		action: [ target moveMethodToPackage ];
		order: 1600.
		
	(aBuilder item: #'Remove...')
		keyText: 'x, m' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removeMethods ];
		order: 1700;
		icon: (self iconNamed: #removeIcon);
		withSeparatorAfter.
		
	(aBuilder item: #'Compare two methods')		 
		action: [ target compareTwoMethods ];
		order: 1800;
		enabledBlock: [ target enableMethodDualSelection ];
		withSeparatorAfter.	
		
	(aBuilder item: #'Inspect Method')
		action: [ target inspectMethod ];
		order: 1950.
		
	(aBuilder item: #'Toggle Bytecodes')
		keyText: 'b, b' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [ target toggleBytecodes ];
		order: 1970.
			
	(aBuilder item: #'File Out')
		action: [ target fileOutMethods ];
		order: 2000