*renraku
select: selectBlock thenDo: doBlock ifNone: alternativeBlock
    "same as #select:thenDo: but also allows to evaluate an alternative block if nothing was selected"
	| found |

	found := false.

    self do: [: each |
        ( selectBlock value: each ) 
			ifTrue: [
				found := true.
				doBlock value: each ]
    ].

	found ifFalse: alternativeBlock