evaluating
value: anObject

  (receiver class isObsolete) ifTrue: [^ nil].
  (receiver isBehavior and: [receiver isObsolete]) ifTrue: [^ nil].

  ^ receiver 
    perform: selector 
    with: anObject