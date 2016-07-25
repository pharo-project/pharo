accessing
projectVersionEscaped
  | pv |
  pv := self projectVersion.
  (projectVersion includes: $/)
    ifTrue: [ ^ pv copyReplaceAll: '/' with: '\/' ].
  ^ pv