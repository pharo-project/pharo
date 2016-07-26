evaluating
value
  "Send the message and answer the return value"

  (receiver class isObsolete) ifTrue: [^ nil].
  (receiver isBehavior and: [receiver isObsolete]) ifTrue: [^ nil].

  arguments ifNil: [^ receiver perform: selector].

  ^ receiver 
    perform: selector 
    withArguments: (self collectArguments: arguments)