accessing
defaultPropertyFileExtension: aString
  "self defaultPropertyFileExtension:'.ston'"

  self validatePropertyFileExtension: aString.
  defaultPropertyFileExtension := aString