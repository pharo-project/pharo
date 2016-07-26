accessing
baselineProjectSpec: anObject
  "force the registration to be consistent -- difficult for Metacello to 
   repair registrations during load -- I've tried. "

  "https://github.com/dalehenrich/metacello-work/issues/212"

  self shouldBeMutable.
  configurationProjectSpec := nil.
  self assert: anObject isBaselineOfProjectSpec.
  baselineProjectSpec := anObject