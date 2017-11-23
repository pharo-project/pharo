"
Command to copy a baseline in Versionner
"
Class {
	#name : #MBCopyBaselineCommand,
	#superclass : #MBVersionCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #execute }
MBCopyBaselineCommand >> copyBaselineWithNumber: baselineNumberAsString [
	"Copy myself into a new baseline, with a different number"

	| comment |
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
]

{ #category : #documentation }
MBCopyBaselineCommand >> documentation [
	^ 'Copy the selected baseline under a new name'
]

{ #category : #execute }
MBCopyBaselineCommand >> execute [

	| answer |
	answer := self uiManager request: 'give a new baseline number (the prefix "-baseline" will be added)'.
	answer ifNil: [ ^ self ].

	self copyBaselineWithNumber: answer


]

{ #category : #accessing }
MBCopyBaselineCommand >> title [
	^ 'copy baseline'
]
