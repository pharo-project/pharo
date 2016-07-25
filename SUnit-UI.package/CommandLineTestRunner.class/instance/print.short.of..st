helper
print: longErrorMessage short: shortErrorMessage of: aTestCase 
	| cr |

	cr := OSPlatform current lineEnding.
	self stdout 
		<< (shortErrorMessage trimBoth padRightTo: 10)
		<< aTestCase printString
		<< cr.
	longErrorMessage isEmptyOrNil 
		ifFalse: [ self stdout << longErrorMessage << cr ]