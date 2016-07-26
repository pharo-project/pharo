private
analyseSeq: seq
	"Checks if consecutive messages have the same receiver. If not, creates a cascade"

	| currentCascade |
	currentCascade := OrderedCollection new.
	seq statements
		do: [ :each | 
			(each isMessage and: [ each selector ~= #ifTrue:ifFalse: ])
				ifFalse: [ self rewriteSeq: seq with: currentCascade ]
				ifTrue: [ 
					currentCascade
						ifNotEmpty: [ 
							currentCascade last receiver = each receiver
								ifFalse: [ self rewriteSeq: seq with: currentCascade ] ].
					currentCascade add: each ] ].
	self rewriteSeq: seq with: currentCascade