printing
printCompilerWarning: aSyntaxErrorNotification
	| stderr position contents errorLine errorMessage maxLineNumberSize lineNumber |
	
	"format the error"
	position := aSyntaxErrorNotification location.
	contents := aSyntaxErrorNotification errorCode.
	errorLine := contents lineNumberCorrespondingToIndex: position.
	stderr := VTermOutputDriver stderr.
		
	"first gather the error title to be able to underline it properly"
	errorMessage := String streamContents: [ :s|
		s nextPutAll: 'Syntax Error on line '; 
			print: errorLine; nextPutAll: ': '; 
			print: aSyntaxErrorNotification errorMessage].
	
	 stderr red;
		nextPutAll: errorMessage; lf;
		nextPutAll: ('' padLeftTo: errorMessage size with: $=); lf;
		clear.
	
	"print each source line and mark the found syntax error"
	maxLineNumberSize := contents lines size asString size.
	lineNumber := 0.
	contents lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineNumber := lineNumber + 1.
		lineNumber == errorLine ifTrue: [ stderr errorColor ].
		"0 pad the line numbers to the same size"
		stderr 
			nextPutAll: (lineNumber asString padLeftTo: maxLineNumberSize with: $0);
			nextPutAll: ': ';
			nextPutAll: (contents copyFrom: start to: endWithoutDelimiters);
			lf.
			"print the marker under the error line"
		(lineNumber == errorLine)
			ifTrue: [
					stderr nextPutAll:( '_^_' padLeftTo: position - start + maxLineNumberSize + 4);
							 lf;
							 clear]
	]