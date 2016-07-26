evaluating
valueWithArguments: anArray

	"Return the last result"

    | answer |
    self do:
        [:each |
        	each isValid ifTrue: [answer := each valueWithArguments: anArray]].
    ^answer