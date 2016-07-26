private
execute: selector args: args
  | script |
  script := self statements copy.
  script add: selector -> args.
  ^ self scriptExecutor execute: script