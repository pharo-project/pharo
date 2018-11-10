Extension { #name : #LGitDiff }

{ #category : #'*Iceberg-Core' }
LGitDiff >> files [
	| deltaFiles |
	
	deltaFiles := Set new.
	self filesDo: [ :delta :ignored1 :ignored2 | 
		deltaFiles add: delta newFile path.
		0 "Continue" ].
	^ deltaFiles
]

{ #category : #'*Iceberg-Core' }
LGitDiff >> filesDo: aBlockClosure [ 
	^ self filesDo: aBlockClosure binaryFilesDo: aBlockClosure hunksDo: nil linesDo: nil		

]

{ #category : #'*Iceberg-Core' }
LGitDiff >> includesFileNamed: path [
	^ (self filesDo: [ :delta :second :third | 
		| modifiedFileName |
		modifiedFileName := delta newFile path.
		(modifiedFileName beginsWith: path) asAlien
	]) asBoolean.

]
