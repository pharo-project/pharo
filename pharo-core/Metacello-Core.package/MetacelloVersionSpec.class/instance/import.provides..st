accessing
import: aString provides: anArray
  importArray := importArray
    ifNil: [ {(aString -> anArray)} ]
    ifNotNil: [ importArray , {(aString -> anArray)} ]