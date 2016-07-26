*GT-SpotterExtensions-Core
spotterForGlobalVarsFor: aStep
	<spotterOrder: 40>
	aStep listProcessor
			allCandidates: [ Smalltalk globals associations select:[:assoc | 
			assoc value isBehavior not and:[ 
				assoc value isTrait not]] ];
			title: 'Global variables';
			itemName:[:global | global key ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false