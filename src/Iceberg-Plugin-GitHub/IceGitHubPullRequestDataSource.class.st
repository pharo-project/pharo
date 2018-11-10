"
I'm a datasource for pull request status, to be used when showing pull requests details. 

See IceGitHubPullRequestDetailModel
"
Class {
	#name : #IceGitHubPullRequestDataSource,
	#superclass : #IceGitHubJSONDataSource,
	#category : #'Iceberg-Plugin-GitHub-Utils'
}

{ #category : #accessing }
IceGitHubPullRequestDataSource >> actionColumn: column [
	column id = #target_url ifTrue: [ ^ self ].
	^ super actionColumn: column
	
]

{ #category : #accessing }
IceGitHubPullRequestDataSource >> cellColumn: column row: rowIndex [ 
	column id = #target_url ifTrue: [ ^ self targetUrlCellColumn: column rowIndex: rowIndex ].
	column id = #state ifTrue: [ ^ self statusCellColumn: column rowIndex: rowIndex ].
	column id = #context ifTrue: [ ^ self contextCellColumn: column rowIndex: rowIndex ].
	^ super cellColumn: column row: rowIndex 
]

{ #category : #private }
IceGitHubPullRequestDataSource >> columnTitleFor: column [
	column id = #target_url ifTrue: [ ^ '' ].
	^ super columnTitleFor: column
]

{ #category : #private }
IceGitHubPullRequestDataSource >> contextCellColumn: column rowIndex: rowIndex [
	| context |
	context := (self elementAt: rowIndex) at: column id.
	(context includesSubstring: 'travis-ci')
		ifTrue: [ context := 'travis-ci' ].
	^ FTCellMorph new  
		addMorphBack: context asMorph;
		yourself
		
]

{ #category : #actions }
IceGitHubPullRequestDataSource >> copyToClipboard: url [
	Clipboard clipboardText: url informing: ('Url {1} copied to clipboard' format: {url})
]

{ #category : #private }
IceGitHubPullRequestDataSource >> iconForStatus: status [
	^ self iconNamed: (self iconNameForStatus: status)
]

{ #category : #private }
IceGitHubPullRequestDataSource >> iconNameForStatus: status [
	status = 'success' ifTrue: [ ^ #testGreen ].
	status = 'failure' ifTrue: [ ^ #testRed ].
	status = 'pending' ifTrue: [ ^ #testYellow ].
	
	^ #testNotRun
]

{ #category : #private }
IceGitHubPullRequestDataSource >> statusCellColumn: column rowIndex: rowIndex [
	| status |
	status := (self elementAt: rowIndex) at: column id.
	^ FTCellMorph new  
		cellInset: 5;
		addMorphBack: (self iconForStatus: status) asMorph;
		addMorphBack: status asStringMorph;
		yourself
	
]

{ #category : #private }
IceGitHubPullRequestDataSource >> targetUrlCellColumn: column rowIndex: rowIndex [
	| url |
	url := (self elementAt: rowIndex) at: column id.
	^ FTCellMorph new  
		addMorphBack: (
			IconicButton new
				target: self;
				actionSelector: #copyToClipboard:;
				arguments: { url };
				labelGraphic: ((self iconNamed: #smallCopy) scaledToSize: 12@12);
				color: Color transparent;
				helpText: ('Copy url {1} to clipboard' format: { url });
				borderWidth: 0;
				yourself);
		yourself
	
]
