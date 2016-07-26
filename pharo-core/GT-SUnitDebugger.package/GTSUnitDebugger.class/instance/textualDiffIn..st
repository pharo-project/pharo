building presentations
textualDiffIn: composite

	^ composite diff
			when: [ self session supportsDiff];
			display: [ 
				{self session actualResult gtDebuggerSUnitPrint. self session expectedResult gtDebuggerSUnitPrint} ]