private
repositoryString
    | repositoryString |
    repositoryString := ''.
    self repositories do: [ :repo | repositoryString := repositoryString , ' ' , repo description ].
    ^ repositoryString