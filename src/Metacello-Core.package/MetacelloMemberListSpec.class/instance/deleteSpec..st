toolbox support
deleteSpec: aSpec
    "remove the spec from list"

    | member |
    member := self list detect: [ :aMember | aMember spec = aSpec ] ifNone: [ ^ self ].
    self list remove: member