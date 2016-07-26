accessing
choosePage
	| choiceList chosen |
	choiceList := OrderedCollection new.
	self allIntervals 
		doWithIndex: [:assoc :idx | | choiceString |
			choiceString := idx asString, ': ', (self nodeList at: assoc key) asString, ' ... ', (self nodeList at: assoc value) asString.
			choiceList add: idx -> choiceString].
	chosen := 
		UIManager default 
			chooseFrom: (choiceList collect: [:c | c value])
			values: (choiceList collect: [:c | c key])
			lines: nil 
			title: 'Choose a page'.
	chosen ifNil: [^self].
	self currentPage: chosen.