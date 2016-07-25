attributes
defaultPlatformAttributes
  | versionString |
  Smalltalk
    at: #'SystemVersion'
    ifPresent: [ :cl | 
      versionString := cl current version.
      (versionString beginsWith: 'Pharo')
        ifTrue: [ ^ #(#'squeakCommon' #'pharo') ].
      (versionString beginsWith: 'Squeak')
        ifTrue: [ ^ #(#'squeakCommon' #'squeak') ].	"see http://code.google.com/p/metacello/issues/detail?id=146"
      (self string: versionString includesSubstring: 'Pharo')
        ifTrue: [ ^ #(#'squeakCommon' #'pharo') ].
      (self string: versionString includesSubstring: 'Squeak')
        ifTrue: [ ^ #(#'squeakCommon' #'squeak') ].
      self error: 'Unrecognized version of Squeak/Pharo: ' , versionString ].
  ^ #(#'gemstone')