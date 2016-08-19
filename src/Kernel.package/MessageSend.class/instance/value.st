evaluating
value
  "Send the message and answer the return value"

  arguments ifNil: [^ receiver perform: selector].

  ^ receiver 
    perform: selector 
    withArguments: (self collectArguments: arguments)