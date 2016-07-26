*GT-SpotterExtensions-Core
spotterForPragmasFor: aStep
	<spotterOrder: 40>
	aStep listProcessor 
			allCandidates: [ PragmaType all ];
			title: 'Pragmas';
			itemName: [ :pragma | pragma keyword ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false