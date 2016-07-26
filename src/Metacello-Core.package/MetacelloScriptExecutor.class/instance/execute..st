execution
execute: statements
  statements
    do: [ :assoc | 
      assoc value
        ifNil: [ self perform: assoc key ]
        ifNotNil: [ self perform: assoc key withArguments: assoc value ] ].
  projectSpecGenerator := self projectSpecGenerator.
  projectSpecGenerator target
    execute: [ :projectSpec | 
      | engine |
      engine := MetacelloScriptEngine new
        options: self options copy;
        projectSpec: projectSpec;
        yourself.
      engine perform: actionArg key withArguments: actionArg value.
      engine root ifNotNil: [ :root | self roots add: root ] ]
    against: self.
  ^ (self singleRoot and: [ self roots size == 1 ])
    ifTrue: [ self roots first ]
    ifFalse: [ self roots ]