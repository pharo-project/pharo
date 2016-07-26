public api
findMessage
	"Control the search."

	data do: [:alist |
		(alist isKindOf: SequenceableCollection) ifFalse: [
			^ OrderedCollection with: 'first and third items are not Arrays']].
	Approved ifNil: [self initialize].	"Sets of allowed selectors"
	expressions := OrderedCollection new.
	self search: true.	"multi"
	expressions isEmpty ifTrue: [^ OrderedCollection with: 'no single method does that function'].
	expressions isString ifTrue: [^ OrderedCollection with: expressions].
 	^ expressions