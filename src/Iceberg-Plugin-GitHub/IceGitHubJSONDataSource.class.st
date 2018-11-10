"
I'm a generic datasource to show in a table results of a JSON. 
I'm intended to be used to show information from GitHub API, but I'm generic enough to be used which any kind of JSON structure.
"
Class {
	#name : #IceGitHubJSONDataSource,
	#superclass : #FTSimpleDataSource,
	#category : #'Iceberg-Plugin-GitHub-Utils'
}

{ #category : #accessing }
IceGitHubJSONDataSource >> actionColumn: column [
	elements := elements sorted: [ :a :b |
		(a at: column id) < (b at: column id) ]
		
]

{ #category : #accessing }
IceGitHubJSONDataSource >> cellColumn: column row: rowIndex [ 
	^ FTCellMorph new  
		addMorphBack: ((self elementAt: rowIndex) at: column id) asStringMorph;
		yourself
	
]

{ #category : #private }
IceGitHubJSONDataSource >> columnTitleFor: column [
	^ column id capitalized
]

{ #category : #accessing }
IceGitHubJSONDataSource >> headerColumn: column [
	column id ifNil: [ ^ nil ].

	^ SimpleButtonMorph new
		layoutPolicy: RowLayout new; 
		listDirection: #leftToRight; 
		wrapCentering: #center;
		cellInset: 5@0;
		layoutInset: 5@0;
		label: (self columnTitleFor: column);
		target: self;
		actionSelector: #actionColumn:;
		arguments: {column};
		yourself
]
