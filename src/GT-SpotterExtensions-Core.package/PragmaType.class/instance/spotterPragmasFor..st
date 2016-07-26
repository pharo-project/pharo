ui
spotterPragmasFor: aStep
	<spotterOrder: 40>
	^ aStep listProcessor
		title: 'Pragmas';
		allCandidates: [ self pragmas ];
		itemName: [ :pragma | pragma method printString , ' ' , pragma asString ];
		filter: GTFilterSubstring