history saving
showProgress
	| testCaseClasses d string |
	testCaseClasses := (self suiteForAllSelected tests collect: [:testCase | testCase class]) asSet.
	
	"At the end of the algorithm, d will contains all the diff between what was saved and the current result"
	d := Dictionary new.
	d at: #passed put: OrderedCollection new.
	d at: #failures put: OrderedCollection new.
	d at: #errors put: OrderedCollection new.

	testCaseClasses do: [ :cls | | t |
		(cls class includesSelector: #lastStoredRun)
			ifTrue: [t := cls lastStoredRun.
					(t at: #passed) do: [:s | 	
											(result isErrorFor: cls selector: s)
												ifTrue: [(d at: #errors) add: {cls . s}].
											(result isFailureFor: cls selector: s)
												ifTrue: [(d at: #failures) add: {cls . s}]  ].
											
					(t at: #failures) do: [:s | (result isPassedFor: cls selector: s)
												ifTrue: [(d at: #passed) add: {cls . s}].
											(result isErrorFor: cls selector: s)
												ifTrue: [(d at: #errors) add: {cls . s}]].
											
					(t at: #errors) do: [:s | 	(result isPassedFor: cls selector: s)
												ifTrue: [(d at: #passed) add: {cls . s}].
											(result isFailureFor: cls selector: s)
												ifTrue: [(d at: #failures) add: {cls . s}]]]].
		
			
	string := String streamContents: [:str|
		str nextPutAll: '----------------'; cr.
		str nextPutAll: 'Diff between current result and saved result'; cr.
		str nextPutAll: 'New passed: '.
		(d at: #passed) do: [:s| str nextPutAll: s printString, ' '].
		str cr.
		str nextPutAll: 'New failures: '.
		(d at: #failures) do: [:s| str nextPutAll: s printString, ' '].
		str cr.
		
		str nextPutAll: 'New errors: '.
		(d at: #errors) do: [:s| str nextPutAll: s printString, ' '].
		str cr].
	
	UIManager default edit: string label: 'SUnit Progress'. 

	