history saving
showDiffWith: aTestResult
	| string diff |

	diff := result diff: aTestResult.
	string := String streamContents: [:str|
		str nextPutAll: '----------------'; cr.
		str nextPutAll: 'Diff between current result with: ', aTestResult asString; cr.
		str nextPutAll: 'New passed: '.
		diff first do: [:s| str nextPutAll: s printString, ' '].
		str cr.
		str nextPutAll: 'New failures: '.
		diff second do: [:s| str nextPutAll: s printString, ' '].
		str cr.
		
		str nextPutAll: 'New errors: '.
		diff third do: [:s| str nextPutAll: s printString, ' '].
		str cr].
	
	UIManager default edit: string label: 'SUnit Progress'. 
	