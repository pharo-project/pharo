private
ensureReceiverAndArguments: aReceiver

  "Return true if my receiver hasn't gone away"
  aReceiver ifNil: [^ false].
  (aReceiver class isObsolete) ifTrue: [^ false].
  (aReceiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].

  "Make sure that my arguments haven't gone away"
  arguments ifNotNil: [
    arguments with: shouldBeNil do: [ :arg :flag |
      arg ifNil: [ flag ifFalse: [ ^false ]]
    ]
  ].

  ^true
