execute
copyBaselineWithNumber: baselineNumberAsString
	"Copy myself into a new baseline, with a different number"

	| sourceCode newSource newNumber newSelector comment |
	self 
		assert: [ baselineNumberAsString ~= target versionString ] 
		description: 'Cannot assign the same baseline number than me to my copy'.
		
	self 
		assert: [ (baselineNumberAsString endsWith: 'baseline') not ] 
		description: 'The baseline number cannot end with "-baseline". It will be automatically added.'.

	comment := self uiManager request: 'Enter a comment for the new baseline'.
	MetacelloToolBox 
		createBaseline: baselineNumberAsString, '-baseline' 
		for: target configurationBasename 
		from: target versionString 
		description: comment.