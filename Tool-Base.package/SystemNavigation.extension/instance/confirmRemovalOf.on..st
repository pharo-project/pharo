*Tool-Base
confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count answer caption allCalls |
	allCalls := self allCallsOn: aSelector.
	(count := allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first selector == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	caption := 'The message ', aSelector printString ,' has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption := caption copyWith: $s].
	answer := UIManager default 
		chooseFrom: #('Remove it'
				'Remove, then browse senders'
				'Don''t remove, but show me those senders'
				'Forget it -- do nothing -- sorry I asked') title: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoSelect: aSelector keywords first].
	answer = 0
		ifTrue: [answer := 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3