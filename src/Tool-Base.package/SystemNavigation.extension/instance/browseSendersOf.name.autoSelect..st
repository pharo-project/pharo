*Tool-Base
browseSendersOf: aSelector name: labelString autoSelect: autoSelectString
	
	^ self
		browseMessageList: (self allCallsOn: aSelector)
		name: labelString
		autoSelect: autoSelectString
		refreshingBlock: [ :method | method hasSelector: aSelector ]