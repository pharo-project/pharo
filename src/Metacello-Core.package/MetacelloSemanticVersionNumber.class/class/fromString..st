instance creation
fromString: aString
  | svn |
  svn := self fromString: aString forPattern: false.
  self validateVersionNumber: svn against: aString.
  ^ svn