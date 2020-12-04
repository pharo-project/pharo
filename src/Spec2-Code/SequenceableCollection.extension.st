Extension { #name : #SequenceableCollection }

{ #category : #'*Spec2-Code' }
SequenceableCollection >> groupsOf: n [
	| groups pos |

	pos := 0.
	groups := OrderedCollection new.
	(n to: self size by: n) do: [ :index | 
			| args |
			args := self copyFrom: index - n + 1 to: index.
			groups add: args.
			pos := pos + n ].
	pos < self size ifTrue: [ 
		groups add: (self copyFrom: pos + 1 to: self size) ].
	^ groups
]
