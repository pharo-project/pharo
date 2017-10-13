private
validateVersionNumber: svn against: aString
  svn printString = aString
    ifFalse: [ 
      self
        error:
          'The printString of a semantic version number should be equal to the source version string' ]