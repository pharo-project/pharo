private
sourceSearch: aSearchString
	"I'm searching in sources"
	^ self useRegEx
		ifTrue: [ self sourceRegexSearch: aSearchString ]
		ifFalse:[ self sourceStringSearch: aSearchString ].