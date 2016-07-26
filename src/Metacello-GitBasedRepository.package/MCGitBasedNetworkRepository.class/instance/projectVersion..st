accessing
projectVersion: aString
  "Important that projectVersion be set AFTER projectPath, as projectPath needed for resolving projectVersionPattern"

  | x |
  aString
    ifNil: [ 
      projectVersion := aString.
      ^ self ].
  (x := aString
    findDelimiters:
      {$#.
      $*.
      $?}
    startingAt: 1) <= aString size
    ifTrue: [ self resolveProjectVersionPattern: aString ]
    ifFalse: [ projectVersion := aString ]