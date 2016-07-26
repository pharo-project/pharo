tests
testWhenListChanged
	"Two stages list changed notification."

	50
		timesRepeat: [ 
			| list ok |
			list := ListModel new.
			list items: #(1 2).
			list whenListChanged: [ :aList | ok := aList = list listItems ].
			list listItems.	"Call listItems to set the cache"
			list items: #(1 2 3).
			self assert: ok ]