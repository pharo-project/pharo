accessing
validatePropertyFileExtension: aString
  "see Issue #90: https://github.com/dalehenrich/filetree/issues/90"

  (#('.json' '.ston') includes: aString)
    ifFalse: [ self error: 'Unsupported property file extension: ' , aString printString ]