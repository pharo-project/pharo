evaluating
value
    "Answer the result of evaluating the elements of the receiver.
	Actually, return just the last result."

    | answer |
    self do:
        [:each | each isValid ifTrue: [answer := each value]].
    ^answer