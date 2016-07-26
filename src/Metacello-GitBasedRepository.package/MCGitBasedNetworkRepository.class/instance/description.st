descriptions
description
  | desc |
  desc := self class description , self projectPath , ':'
    , self projectVersionEscaped.
  self repoPath isEmpty
    ifTrue: [ ^ desc ].
  ^ desc , '/' , self repoPath