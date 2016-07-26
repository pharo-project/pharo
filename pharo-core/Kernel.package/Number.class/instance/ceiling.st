truncation and round off
ceiling
       "Answer the integer nearest the receiver toward  infinity."

       | truncation |
       truncation := self truncated.
       self <= 0 ifTrue: [ ^ truncation ].
       ^ self = truncation
               ifTrue: [ truncation ]
               ifFalse: [ truncation + 1 ]