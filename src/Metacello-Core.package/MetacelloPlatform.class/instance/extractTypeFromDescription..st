repository creation
extractTypeFromDescription: description
  description == nil
    ifTrue: [ ^ nil ].
  ((description beginsWith: '/') or: [ description second = $: ])
    ifTrue: [ ^ 'directory' ].
  (description beginsWith: 'dictionary://')
    ifTrue: [ ^ 'dictionary' ].
  (description beginsWith: 'filetree://')
    ifTrue: [ ^ 'filetree' ].
  (description beginsWith: 'tonel://')
    ifTrue: [ ^ 'tonel' ].
  (description beginsWith: 'github://')
    ifTrue: [ ^ 'github' ].
  (description beginsWith: 'gitorious://')
    ifTrue: [ ^ 'gitorious' ].
  (description beginsWith: 'bitbucket://')
    ifTrue: [ ^ 'bitbucket' ].
  ^ 'http'