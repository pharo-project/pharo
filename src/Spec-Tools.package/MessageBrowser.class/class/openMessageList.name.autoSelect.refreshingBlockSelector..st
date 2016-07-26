instance creation - old
openMessageList: messageList name: aString autoSelect: aSelector refreshingBlockSelector: aRefreshingBlockSelector
	
	| browser |
	browser := self new
		messages: messageList;
		title: aString;
		autoSelect: aSelector;
		yourself.
		
	aRefreshingBlockSelector ifNotNil:[(browser perform: aRefreshingBlockSelector with: aSelector)].
	^browser open
	
				
