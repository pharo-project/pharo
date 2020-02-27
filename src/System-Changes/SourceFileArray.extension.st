Extension { #name : #SourceFileArray }

{ #category : #'*System-Changes' }
SourceFileArray >> changeRecordsFor: aMethodDefinition [

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer 
		className: aMethodDefinition methodClass instanceSide name
		isMeta: aMethodDefinition methodClass isMeta
]

{ #category : #'*System-Changes' }
SourceFileArray >> changeRecordsFor: aMethodDefinition detect: aUnaryBlock [
	"Try to detect the most recent ChangeRecord that satisfies aUnaryBlock. Answer nil if none satisfies."

	self
		changeRecordsFor: aMethodDefinition
		do: [ :changeRecord | 
			(aUnaryBlock value: changeRecord category) 
				ifTrue: [ ^ changeRecord ] ].
	^ nil
]

{ #category : #'*System-Changes' }
SourceFileArray >> changeRecordsFor: aMethodDefinition do: aUnaryBlock [
	"Evaluate aUnaryBlock with each of the ChangeRecords of aMethodDefinition. Most recent changes are evaluated first."

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer
		className: aMethodDefinition instanceSideParentName
		isMeta: aMethodDefinition isMetaSide
		do: aUnaryBlock
]

{ #category : #'*System-Changes' }
SourceFileArray >> changeRecordsFrom: initialSourcePointer className: theNonMetaClassName isMeta: classIsMeta [
	"Answer the ChangeRecords of a method or class comment, starting from the initialSourcePointer. 
	Most recent changes go first."
	
	| changeRecords |
	changeRecords := OrderedCollection new.

	self
		changeRecordsFrom: initialSourcePointer
		className: theNonMetaClassName
		isMeta: classIsMeta
		do: [ :changeRecord | changeRecords add: changeRecord ].
	
	^ changeRecords
]

{ #category : #'*System-Changes' }
SourceFileArray >> changeRecordsFrom: initialSourcePointer className: theNonMetaClassName isMeta: classIsMeta do: aUnaryBlock [
	"Evaluate aUnaryBlock with each of the ChangeRecords of a method or class comment, starting from the initialSourcePointer. 
	Most recent changes are evaluated first."

	| filePosition fileIndex |
	fileIndex := self fileIndexFromSourcePointer: initialSourcePointer.
	filePosition := self filePositionFromSourcePointer: initialSourcePointer.	

	self readOnlyDo: [ :sourceFilesCopy | 
		| file previousFilePosition previousFileIndex preamble stamp protocol preambleTokens |
		file := sourceFilesCopy fileAt: fileIndex ifAbsent: [ ^ self ].
		
		[ filePosition notNil & file notNil ]
			whileTrue: [ 
				file position: (0 max: filePosition - 150).	"Skip back to before the preamble"
				[ file position < (filePosition - 1) ] whileTrue: [ 
					preamble := (ChunkReadStream on: file) next ].	"then pick it up from the front"	
					"Preamble is likely a linked method preamble, if we're in
					a changes file (not the sources file).  Try to parse it
					for prior source position and file index"

				previousFilePosition := nil.
				stamp := ''.
				"method records"
				(preamble includesSubstring: 'methodsFor:')
					ifTrue: [ preambleTokens := preamble parseLiterals ]
					ifFalse: [ preambleTokens := Array new	"ie cant be back ref" ].
				((preambleTokens size between: 7 and: 8) and: [ (preambleTokens at: preambleTokens size - 5) = #methodsFor: ])
					ifTrue: [ 
						(preambleTokens at: preambleTokens size - 3) = #stamp:
							ifTrue: [ 
								"New format gives change stamp and unified prior pointer"
								stamp := preambleTokens at: preambleTokens size - 2.
								previousFilePosition := preambleTokens last.
								previousFileIndex := self fileIndexFromSourcePointer: previousFilePosition.
								previousFilePosition := self filePositionFromSourcePointer: previousFilePosition ]
							ifFalse: [ 
								"Old format gives no stamp; prior pointer in two parts"
								previousFilePosition := preambleTokens at: preambleTokens size - 2.
								previousFileIndex := preambleTokens last ].
						(previousFilePosition = 0 or: [ previousFileIndex = 0 ])
							ifTrue: [ previousFilePosition := nil ] ].

				((preambleTokens size between: 5 and: 6) and: [ (preambleTokens at: preambleTokens size - 3) = #methodsFor: ])
					ifTrue: [ 
						(preambleTokens at: preambleTokens size - 1) = #stamp:
							ifTrue: [ 
								"New format gives change stamp and unified prior pointer"
								stamp := preambleTokens at: preambleTokens size ] ].
				"class comment records"
				(preamble includesSubstring: 'commentStamp:')
					ifTrue: [ preambleTokens := preamble parseLiterals ].
				((preambleTokens size = 5 or: [ preambleTokens size = 3 ]) and: [ (preambleTokens at: 2) = #commentStamp: ])
					ifTrue: [ 
						stamp := preambleTokens at: 3.
						preambleTokens size > 3
							ifTrue: [ 
								previousFilePosition := preambleTokens last.
								previousFileIndex := self fileIndexFromSourcePointer: previousFilePosition.
								previousFilePosition := self filePositionFromSourcePointer: previousFilePosition ].
						aUnaryBlock
							value:
								(ChangeRecord new
									file: file
									position: filePosition
									type: #classComment
									class: theNonMetaClassName
									category: nil
									meta: classIsMeta
									stamp: stamp) ]
					ifFalse: [ 
						protocol := preambleTokens after: #methodsFor: ifAbsent: [ Protocol unclassified ].
						aUnaryBlock
							value:
								(ChangeRecord new
									file: file
									position: filePosition
									type: #method
									class: theNonMetaClassName
									category: protocol
									meta: classIsMeta
									stamp: stamp) ].
									
				filePosition := previousFilePosition.
				previousFilePosition ifNotNil: [ 
					file := sourceFilesCopy 
						fileAt: previousFileIndex
						ifAbsent: [ ^ self ] ] ] ]
]
