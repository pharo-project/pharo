comparing
hash

"Returns a numeric hash key for the receiver."

| mySize interval hashValue |

(mySize := self size) == 0
  ifTrue: [ ^15243 ].

"Choose an interval so that we sample at most 5 elements of the receiver"
interval := ((mySize - 1) // 4) max: 1.

hashValue := 4459.
1 to: mySize by: interval do: [ :i | | anElement |
  anElement := self at: i.
  (anElement isKindOf: SequenceableCollection)
    ifTrue: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement size.
      ]
    ifFalse: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement hash.
      ].
  ].

^ hashValue abs