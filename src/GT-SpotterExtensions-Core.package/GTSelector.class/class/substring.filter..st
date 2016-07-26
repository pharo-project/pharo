private
substring: aString filter: aFilter
	| selectors |
	aString isEmptyOrNil ifTrue: [ ^ self ].
	selectors := IdentityDictionary new.
	SystemNavigation default allBehaviorsDo: [ :class | 
		class selectorsAndMethodsDo: [ :selector :method | 
			(selector asString includesSubstring: aString caseSensitive: false) ifTrue: [ 
				| group newGroup | 
				group := selectors 
					at: selector 
					ifAbsentPut: [ aFilter value: (newGroup := GTSelector new name: selector; yourself). newGroup ].
				group addMethod: method ] ] ] 