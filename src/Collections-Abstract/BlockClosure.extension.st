Extension { #name : #BlockClosure }

{ #category : #'*Collections-Abstract-splitjoin' }
BlockClosure >> split: aSequenceableCollection indicesDo: aBlock [
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of aSequenceableCollection that have been identified by taking the receiver as a splitter." 
	
	"(String streamContents: [:s | [:c | c isSeparator ] split:  'Pharo is cool'  indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>>  's:1 e:5 s:7 e:8 s:10 e:13 '"
	
	
	| position |
	
	position := 1.
	
	aSequenceableCollection withIndexDo: [:element :idx |
		(self value: element) 
			ifTrue: [
				aBlock value: position value: idx - 1.
				position := idx + 1 ]].
		
	aBlock value: position value: aSequenceableCollection size
]
