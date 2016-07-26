tests
testSetMaximumSizeShrink
	| m |

	m := fullCache instVarNamed: #maximumSize.
	fullCache maximumSize: m // 2 . "shrink"
	self assert: (fullCache instVarNamed: #used) = 0. "cache is cleared when used > maximumSize"
	self validateSizes: fullCache.
	self validateCollections: fullCache.	
		
		