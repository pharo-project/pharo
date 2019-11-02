Extension { #name : #SourceFileArray }

{ #category : #'*Ring-Definitions-Core' }
SourceFileArray >> changeRecordsFor: aMethodDefinition [

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer 
		className: aMethodDefinition methodClass instanceSide name
		isMeta: aMethodDefinition methodClass isMeta
]

{ #category : #'*Ring-Definitions-Core' }
SourceFileArray >> changeRecordsFor: aMethodDefinition detect: aUnaryBlock [
	"Try to detect the most recent ChangeRecord that satisfies aUnaryBlock. Answer nil if none satisfies."

	self
		changeRecordsFor: aMethodDefinition
		do: [ :changeRecord | 
			(aUnaryBlock value: changeRecord category) 
				ifTrue: [ ^ changeRecord ] ].
	^ nil
]

{ #category : #'*Ring-Definitions-Core' }
SourceFileArray >> changeRecordsFor: aMethodDefinition do: aUnaryBlock [
	"Evaluate aUnaryBlock with each of the ChangeRecords of aMethodDefinition. Most recent changes are evaluated first."

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer
		className: aMethodDefinition instanceSideParentName
		isMeta: aMethodDefinition isMetaSide
		do: aUnaryBlock
]
