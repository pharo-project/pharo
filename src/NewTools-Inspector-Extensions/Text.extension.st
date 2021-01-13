Extension { #name : #Text }

{ #category : #'*NewTools-Inspector-Extensions' }
Text >> inspectionText [
	<inspectorPresentationOrder: -10 title: 'Text'>
	
	^ SpTextPresenter new
		text: (self truncateWithElipsisTo: 10000);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
Text >> truncateWithElipsisTo: maxLength [
	"Return myself or a copy suitably shortened but with elipsis added"
	"Copied from String"

	^ self size <= maxLength
		ifTrue:
			[self]
		ifFalse:
			[(self copyFrom: 1 to: (maxLength - 3)), '...']


	"'truncateWithElipsisTo:' asText allBold truncateWithElipsisTo: 20"
]
