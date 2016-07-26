versions
versions
  ^ [ {(self version)} ]
    on: MetacelloVersionDoesNotExistError
    do: [ :ex | ^ #() ]