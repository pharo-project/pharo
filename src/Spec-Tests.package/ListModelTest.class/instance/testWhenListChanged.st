tests
testWhenListChanged
	"Two stages list changed notification."

	50
		timesRepeat: [ | ok |
			testedInstance := ListPresenter new.
			testedInstance items: #(1 2); whenListChanged: [ :aList | ok := aList = testedInstance listItems ]; listItems;	"Call listItems to set the cache"
			 items: #(1 2 3).
			self assert: ok ]