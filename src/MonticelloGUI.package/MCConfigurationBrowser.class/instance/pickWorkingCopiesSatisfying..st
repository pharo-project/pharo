morphic ui
pickWorkingCopiesSatisfying: aBlock 
	| copies item |
	copies := (self allManagers select: aBlock)
				asSortedCollection: [:a :b | a packageName <= b packageName].
	item := UIManager default
				chooseFrom: ({'match ...' translated} , (copies
						collect: [:ea | ea packageName]))
				lines: #(1 )
				title: 'Package:' translated. 
	item = 1
		ifTrue: [| pattern | 
			pattern := UIManager default request: 'Packages matching:' translated initialAnswer: '*'.
			^ pattern isEmptyOrNil
				ifTrue: [#()]
				ifFalse: [(pattern includes: $*)
						ifFalse: [pattern := '*' , pattern , '*'].
					copies
						select: [:ea | pattern match: ea packageName]]].
	^ item = 0
		ifTrue: [#()]
		ifFalse: [{copies at: item - 1}]